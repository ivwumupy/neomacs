//! Render thread implementation.
//!
//! Owns winit event loop, wgpu, GLib/WebKit. Runs at native VSync.

use std::sync::Arc;
use std::thread::{self, JoinHandle};

use winit::application::ApplicationHandler;
use winit::event::{ElementState, KeyEvent, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::core::frame_glyphs::FrameGlyphBuffer;
use crate::thread_comm::{InputEvent, RenderCommand, RenderComms};

/// Render thread state
pub struct RenderThread {
    handle: Option<JoinHandle<()>>,
}

impl RenderThread {
    /// Spawn the render thread
    pub fn spawn(comms: RenderComms, width: u32, height: u32, title: String) -> Self {
        let handle = thread::spawn(move || {
            run_render_loop(comms, width, height, title);
        });

        Self {
            handle: Some(handle),
        }
    }

    /// Wait for render thread to finish
    pub fn join(mut self) {
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

/// Application state for winit event loop
struct RenderApp {
    comms: RenderComms,
    window: Option<Arc<Window>>,
    current_frame: Option<FrameGlyphBuffer>,
    width: u32,
    height: u32,
    title: String,
    // TODO: Add wgpu state, GLib context, etc.
}

impl RenderApp {
    fn new(comms: RenderComms, width: u32, height: u32, title: String) -> Self {
        Self {
            comms,
            window: None,
            current_frame: None,
            width,
            height,
            title,
        }
    }

    /// Process pending commands from Emacs
    fn process_commands(&mut self) -> bool {
        let mut should_exit = false;

        while let Ok(cmd) = self.comms.cmd_rx.try_recv() {
            match cmd {
                RenderCommand::Shutdown => {
                    log::info!("Render thread received shutdown command");
                    should_exit = true;
                }
                RenderCommand::WebKitCreate { id, width, height } => {
                    log::debug!("WebKit create: id={}, {}x{}", id, width, height);
                    // TODO: Create WebKit view
                }
                RenderCommand::WebKitLoadUri { id, url } => {
                    log::debug!("WebKit load: id={}, url={}", id, url);
                    // TODO: Load URL
                }
                RenderCommand::WebKitResize { id, width, height } => {
                    log::debug!("WebKit resize: id={}, {}x{}", id, width, height);
                    // TODO: Resize view
                }
                RenderCommand::WebKitDestroy { id } => {
                    log::debug!("WebKit destroy: id={}", id);
                    // TODO: Destroy view
                }
                RenderCommand::VideoCreate { id, path } => {
                    log::debug!("Video create: id={}, path={}", id, path);
                    // TODO: Create video
                }
                RenderCommand::VideoPlay { id } => {
                    log::debug!("Video play: id={}", id);
                    // TODO: Play video
                }
                RenderCommand::VideoPause { id } => {
                    log::debug!("Video pause: id={}", id);
                    // TODO: Pause video
                }
                RenderCommand::VideoDestroy { id } => {
                    log::debug!("Video destroy: id={}", id);
                    // TODO: Destroy video
                }
            }
        }

        should_exit
    }

    /// Get latest frame from Emacs (non-blocking)
    fn poll_frame(&mut self) {
        // Get the newest frame, discarding older ones
        while let Ok(frame) = self.comms.frame_rx.try_recv() {
            self.current_frame = Some(frame);
        }
    }

    /// Pump GLib events (non-blocking)
    #[cfg(feature = "wpe-webkit")]
    fn pump_glib(&self) {
        // TODO: Implement GLib pumping
        // while glib_ctx.iteration(false) {}
    }

    #[cfg(not(feature = "wpe-webkit"))]
    fn pump_glib(&self) {}

    /// Render the current frame
    fn render(&mut self) {
        if let Some(ref _frame) = self.current_frame {
            // TODO: Implement rendering
            log::trace!("Rendering frame");
        }
    }

    /// Translate winit key to keysym
    fn translate_key(key: &Key) -> u32 {
        match key {
            Key::Named(named) => match named {
                NamedKey::Escape => 0xff1b,
                NamedKey::Enter => 0xff0d,
                NamedKey::Tab => 0xff09,
                NamedKey::Backspace => 0xff08,
                NamedKey::Delete => 0xffff,
                NamedKey::Home => 0xff50,
                NamedKey::End => 0xff57,
                NamedKey::PageUp => 0xff55,
                NamedKey::PageDown => 0xff56,
                NamedKey::ArrowLeft => 0xff51,
                NamedKey::ArrowUp => 0xff52,
                NamedKey::ArrowRight => 0xff53,
                NamedKey::ArrowDown => 0xff54,
                NamedKey::Space => 0x20,
                _ => 0,
            },
            Key::Character(c) => c.chars().next().map(|ch| ch as u32).unwrap_or(0),
            _ => 0,
        }
    }
}

impl ApplicationHandler for RenderApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_none() {
            let attrs = Window::default_attributes()
                .with_title(&self.title)
                .with_inner_size(winit::dpi::PhysicalSize::new(self.width, self.height));

            match event_loop.create_window(attrs) {
                Ok(window) => {
                    log::info!("Render thread: window created");
                    self.window = Some(Arc::new(window));
                    // TODO: Initialize wgpu with this window
                }
                Err(e) => {
                    log::error!("Failed to create window: {:?}", e);
                }
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                log::info!("Window close requested");
                self.comms.send_input(InputEvent::WindowClose);
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                self.width = size.width;
                self.height = size.height;
                self.comms.send_input(InputEvent::WindowResize {
                    width: size.width,
                    height: size.height,
                });
                // TODO: Resize wgpu surface
            }

            WindowEvent::Focused(focused) => {
                self.comms.send_input(InputEvent::WindowFocus { focused });
            }

            WindowEvent::KeyboardInput {
                event:
                    KeyEvent {
                        logical_key, state, ..
                    },
                ..
            } => {
                let keysym = Self::translate_key(&logical_key);
                if keysym != 0 {
                    self.comms.send_input(InputEvent::Key {
                        keysym,
                        modifiers: 0, // TODO: Track modifiers
                        pressed: state == ElementState::Pressed,
                    });
                }
            }

            WindowEvent::MouseInput { state, button, .. } => {
                let btn = match button {
                    MouseButton::Left => 1,
                    MouseButton::Middle => 2,
                    MouseButton::Right => 3,
                    MouseButton::Back => 4,
                    MouseButton::Forward => 5,
                    MouseButton::Other(n) => n as u32,
                };
                self.comms.send_input(InputEvent::MouseButton {
                    button: btn,
                    x: 0.0, // TODO: Track mouse position
                    y: 0.0,
                    pressed: state == ElementState::Pressed,
                    modifiers: 0,
                });
            }

            WindowEvent::CursorMoved { position, .. } => {
                self.comms.send_input(InputEvent::MouseMove {
                    x: position.x as f32,
                    y: position.y as f32,
                    modifiers: 0,
                });
            }

            WindowEvent::MouseWheel { delta, .. } => {
                let (dx, dy) = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => (x, y),
                    winit::event::MouseScrollDelta::PixelDelta(pos) => {
                        (pos.x as f32 / 10.0, pos.y as f32 / 10.0)
                    }
                };
                self.comms.send_input(InputEvent::MouseScroll {
                    delta_x: dx,
                    delta_y: dy,
                    x: 0.0,
                    y: 0.0,
                    modifiers: 0,
                });
            }

            WindowEvent::RedrawRequested => {
                self.render();
            }

            _ => {}
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        // Check for shutdown
        if self.process_commands() {
            event_loop.exit();
            return;
        }

        // Get latest frame from Emacs
        self.poll_frame();

        // Pump GLib for WebKit
        self.pump_glib();

        // Request redraw for VSync
        if let Some(ref window) = self.window {
            window.request_redraw();
        }
    }
}

/// Run the render loop (called on render thread)
fn run_render_loop(comms: RenderComms, width: u32, height: u32, title: String) {
    log::info!("Render thread starting");

    let event_loop = EventLoop::new().expect("Failed to create event loop");
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = RenderApp::new(comms, width, height, title);

    if let Err(e) = event_loop.run_app(&mut app) {
        log::error!("Event loop error: {:?}", e);
    }

    log::info!("Render thread exiting");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::thread_comm::ThreadComms;

    #[test]
    fn test_translate_key_named() {
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Escape)), 0xff1b);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Enter)), 0xff0d);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Tab)), 0xff09);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Backspace)), 0xff08);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Delete)), 0xffff);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Home)), 0xff50);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::End)), 0xff57);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::PageUp)), 0xff55);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::PageDown)), 0xff56);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::ArrowLeft)), 0xff51);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::ArrowUp)), 0xff52);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::ArrowRight)), 0xff53);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::ArrowDown)), 0xff54);
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::Space)), 0x20);
    }

    #[test]
    fn test_translate_key_character() {
        assert_eq!(
            RenderApp::translate_key(&Key::Character("a".into())),
            'a' as u32
        );
        assert_eq!(
            RenderApp::translate_key(&Key::Character("A".into())),
            'A' as u32
        );
        assert_eq!(
            RenderApp::translate_key(&Key::Character("1".into())),
            '1' as u32
        );
    }

    #[test]
    fn test_translate_key_unknown() {
        // Unknown named keys should return 0
        assert_eq!(RenderApp::translate_key(&Key::Named(NamedKey::F1)), 0);
        assert_eq!(RenderApp::translate_key(&Key::Dead(None)), 0);
    }

    #[test]
    fn test_render_thread_creation() {
        // Just test that ThreadComms can be created and split
        let comms = ThreadComms::new().expect("Failed to create ThreadComms");
        let (emacs, render) = comms.split();

        // Verify we can access the channels
        assert!(emacs.input_rx.is_empty());
        assert!(render.cmd_rx.is_empty());
    }
}
