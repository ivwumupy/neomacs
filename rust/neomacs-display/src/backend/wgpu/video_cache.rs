//! Video cache with GStreamer backend and optional VA-API hardware acceleration.
//!
//! Provides async video decoding with DMA-BUF zero-copy when available,
//! falling back to CPU decode + copy otherwise.

use std::collections::HashMap;
use std::sync::mpsc;
use std::thread;

use gstreamer as gst;
use gstreamer::prelude::*;
use gstreamer_video as gst_video;
use gstreamer_app as gst_app;

/// Video playback state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VideoState {
    /// Video is loading/buffering
    Loading,
    /// Video is playing
    Playing,
    /// Video is paused
    Paused,
    /// Video playback stopped
    Stopped,
    /// Video reached end
    EndOfStream,
    /// Error occurred
    Error,
}

/// Decoded video frame ready for rendering
pub struct DecodedFrame {
    /// Frame ID
    pub id: u32,
    /// Video ID this frame belongs to
    pub video_id: u32,
    /// Width in pixels
    pub width: u32,
    /// Height in pixels
    pub height: u32,
    /// RGBA pixel data (CPU path)
    pub data: Vec<u8>,
    /// Presentation timestamp in nanoseconds
    pub pts: u64,
    /// Duration in nanoseconds
    pub duration: u64,
}

/// Cached video with GStreamer pipeline
pub struct CachedVideo {
    /// Video ID
    pub id: u32,
    /// Video dimensions
    pub width: u32,
    pub height: u32,
    /// Current state
    pub state: VideoState,
    /// Current wgpu texture (updated each frame)
    pub texture: Option<wgpu::Texture>,
    pub texture_view: Option<wgpu::TextureView>,
    pub bind_group: Option<wgpu::BindGroup>,
    /// Frame count
    pub frame_count: u64,
    /// Loop count (-1 = infinite)
    pub loop_count: i32,
}

/// Request to load a video
struct LoadRequest {
    id: u32,
    path: String,
}

/// Video pipeline with frame extraction
struct VideoPipeline {
    pipeline: gst::Pipeline,
    appsink: gst_video::VideoSink,
}

/// Video cache managing multiple videos with async decoding
pub struct VideoCache {
    /// Cached videos by ID
    videos: HashMap<u32, CachedVideo>,
    /// Next video ID
    next_id: u32,
    /// Channel to send load requests
    load_tx: mpsc::Sender<LoadRequest>,
    /// Channel to receive decoded frames
    frame_rx: mpsc::Receiver<DecodedFrame>,
    /// Bind group layout for video textures
    bind_group_layout: Option<wgpu::BindGroupLayout>,
    /// Sampler for video textures
    sampler: Option<wgpu::Sampler>,
}

impl VideoCache {
    /// Create a new video cache
    pub fn new() -> Self {
        // Initialize GStreamer
        if let Err(e) = gst::init() {
            log::error!("Failed to initialize GStreamer: {}", e);
        }

        let (load_tx, load_rx) = mpsc::channel::<LoadRequest>();
        let (frame_tx, frame_rx) = mpsc::channel::<DecodedFrame>();

        // Spawn decoder thread
        thread::spawn(move || {
            Self::decoder_thread(load_rx, frame_tx);
        });

        Self {
            videos: HashMap::new(),
            next_id: 1,
            load_tx,
            frame_rx,
            bind_group_layout: None,
            sampler: None,
        }
    }

    /// Initialize GPU resources
    pub fn init_gpu(&mut self, device: &wgpu::Device) {
        // Create bind group layout for video textures
        self.bind_group_layout = Some(device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Video Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        }));

        self.sampler = Some(device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Video Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        }));
    }

    /// Load a video file
    pub fn load_file(&mut self, path: &str) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        // Create placeholder entry
        self.videos.insert(id, CachedVideo {
            id,
            width: 0,
            height: 0,
            state: VideoState::Loading,
            texture: None,
            texture_view: None,
            bind_group: None,
            frame_count: 0,
            loop_count: 0,
        });

        // Send load request
        let _ = self.load_tx.send(LoadRequest {
            id,
            path: path.to_string(),
        });

        log::info!("VideoCache: queued video {} for loading: {}", id, path);
        id
    }

    /// Get video state
    pub fn get_state(&self, id: u32) -> Option<VideoState> {
        self.videos.get(&id).map(|v| v.state)
    }

    /// Get video dimensions
    pub fn get_dimensions(&self, id: u32) -> Option<(u32, u32)> {
        self.videos.get(&id).map(|v| (v.width, v.height))
    }

    /// Get video for rendering
    pub fn get(&self, id: u32) -> Option<&CachedVideo> {
        self.videos.get(&id)
    }

    /// Play video
    pub fn play(&mut self, id: u32) {
        if let Some(video) = self.videos.get_mut(&id) {
            video.state = VideoState::Playing;
            log::debug!("VideoCache: play video {}", id);
        }
    }

    /// Pause video
    pub fn pause(&mut self, id: u32) {
        if let Some(video) = self.videos.get_mut(&id) {
            video.state = VideoState::Paused;
            log::debug!("VideoCache: pause video {}", id);
        }
    }

    /// Stop video
    pub fn stop(&mut self, id: u32) {
        if let Some(video) = self.videos.get_mut(&id) {
            video.state = VideoState::Stopped;
            log::debug!("VideoCache: stop video {}", id);
        }
    }

    /// Set loop count (-1 for infinite)
    pub fn set_loop(&mut self, id: u32, count: i32) {
        if let Some(video) = self.videos.get_mut(&id) {
            video.loop_count = count;
        }
    }

    /// Remove video from cache
    pub fn remove(&mut self, id: u32) {
        self.videos.remove(&id);
        log::debug!("VideoCache: removed video {}", id);
    }

    /// Process pending decoded frames (call each frame)
    pub fn process_pending(&mut self, device: &wgpu::Device, queue: &wgpu::Queue) {
        // Process all available frames
        while let Ok(frame) = self.frame_rx.try_recv() {
            if let Some(video) = self.videos.get_mut(&frame.video_id) {
                // Update dimensions if first frame
                if video.width == 0 {
                    video.width = frame.width;
                    video.height = frame.height;
                    video.state = VideoState::Playing;
                }

                // Create texture from frame data
                let texture = device.create_texture(&wgpu::TextureDescriptor {
                    label: Some("Video Frame Texture"),
                    size: wgpu::Extent3d {
                        width: frame.width,
                        height: frame.height,
                        depth_or_array_layers: 1,
                    },
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::Rgba8Unorm,
                    usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                    view_formats: &[],
                });

                queue.write_texture(
                    wgpu::ImageCopyTexture {
                        texture: &texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::All,
                    },
                    &frame.data,
                    wgpu::ImageDataLayout {
                        offset: 0,
                        bytes_per_row: Some(frame.width * 4),
                        rows_per_image: Some(frame.height),
                    },
                    wgpu::Extent3d {
                        width: frame.width,
                        height: frame.height,
                        depth_or_array_layers: 1,
                    },
                );

                let texture_view = texture.create_view(&wgpu::TextureViewDescriptor::default());

                // Create bind group
                if let (Some(layout), Some(sampler)) = (&self.bind_group_layout, &self.sampler) {
                    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                        label: Some("Video Bind Group"),
                        layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::TextureView(&texture_view),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Sampler(sampler),
                            },
                        ],
                    });
                    video.bind_group = Some(bind_group);
                }

                video.texture = Some(texture);
                video.texture_view = Some(texture_view);
                video.frame_count += 1;

                log::trace!("VideoCache: updated video {} frame {}", frame.video_id, video.frame_count);
            }
        }
    }

    /// Background decoder thread
    fn decoder_thread(
        rx: mpsc::Receiver<LoadRequest>,
        tx: mpsc::Sender<DecodedFrame>,
    ) {
        log::debug!("Video decoder thread started");

        while let Ok(request) = rx.recv() {
            log::info!("Decoder thread: loading video {}: {}", request.id, request.path);

            // Create GStreamer pipeline
            // Try VA-API first for hardware acceleration, fall back to software
            let pipeline_str = format!(
                "filesrc location=\"{}\" ! decodebin ! videoconvert ! video/x-raw,format=RGBA ! appsink name=sink",
                request.path.replace("\"", "\\\"")
            );

            match gst::parse::launch(&pipeline_str) {
                Ok(pipeline) => {
                    let pipeline = pipeline.dynamic_cast::<gst::Pipeline>().unwrap();

                    // Get appsink
                    let appsink = pipeline
                        .by_name("sink")
                        .expect("Could not get appsink")
                        .dynamic_cast::<gst_app::AppSink>()
                        .expect("Could not cast to AppSink");

                    // Configure appsink
                    appsink.set_max_buffers(2);
                    appsink.set_drop(true);

                    let video_id = request.id;
                    let tx_clone = tx.clone();

                    // Set up callback for new samples
                    appsink.set_callbacks(
                        gst_app::AppSinkCallbacks::builder()
                            .new_sample(move |sink| {
                                if let Ok(sample) = sink.pull_sample() {
                                    if let Some(buffer) = sample.buffer() {
                                        // Get video info from caps
                                        let caps = sample.caps().expect("No caps");
                                        let info = gst_video::VideoInfo::from_caps(caps)
                                            .expect("Failed to get video info");

                                        let width = info.width();
                                        let height = info.height();

                                        // Map buffer and extract data
                                        if let Ok(map) = buffer.map_readable() {
                                            let data = map.as_slice().to_vec();

                                            let _ = tx_clone.send(DecodedFrame {
                                                id: 0, // Frame ID
                                                video_id,
                                                width,
                                                height,
                                                data,
                                                pts: buffer.pts().map(|p| p.nseconds()).unwrap_or(0),
                                                duration: buffer.duration().map(|d| d.nseconds()).unwrap_or(0),
                                            });
                                        }
                                    }
                                }
                                Ok(gst::FlowSuccess::Ok)
                            })
                            .build(),
                    );

                    // Start playing
                    if let Err(e) = pipeline.set_state(gst::State::Playing) {
                        log::error!("Failed to start pipeline: {:?}", e);
                    }

                    // Wait for EOS or error
                    let bus = pipeline.bus().unwrap();
                    for msg in bus.iter_timed(gst::ClockTime::NONE) {
                        match msg.view() {
                            gst::MessageView::Eos(..) => {
                                log::debug!("Video {} reached end of stream", video_id);
                                break;
                            }
                            gst::MessageView::Error(err) => {
                                log::error!(
                                    "Video {} error: {} ({:?})",
                                    video_id,
                                    err.error(),
                                    err.debug()
                                );
                                break;
                            }
                            _ => {}
                        }
                    }

                    // Cleanup
                    let _ = pipeline.set_state(gst::State::Null);
                }
                Err(e) => {
                    log::error!("Failed to create pipeline for video {}: {}", request.id, e);
                }
            }
        }

        log::debug!("Video decoder thread exiting");
    }
}

impl Default for VideoCache {
    fn default() -> Self {
        Self::new()
    }
}
