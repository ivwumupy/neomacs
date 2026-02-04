//! Vulkan DMA-BUF import for zero-copy texture sharing.
//!
//! This module implements importing DMA-BUF file descriptors as Vulkan images,
//! then wrapping them as wgpu textures for zero-copy GPU-to-GPU transfers.
//!
//! Required Vulkan extensions:
//! - VK_KHR_external_memory
//! - VK_KHR_external_memory_fd
//! - VK_EXT_external_memory_dma_buf
//! - VK_EXT_image_drm_format_modifier

#[cfg(all(target_os = "linux", feature = "ash"))]
use ash::vk;
#[cfg(all(target_os = "linux", feature = "ash"))]
use std::os::unix::io::RawFd;

/// DRM format codes (fourcc)
#[allow(dead_code)]
pub mod drm_fourcc {
    pub const DRM_FORMAT_ARGB8888: u32 = 0x34325241; // ARGB8888
    pub const DRM_FORMAT_XRGB8888: u32 = 0x34325258; // XRGB8888
    pub const DRM_FORMAT_ABGR8888: u32 = 0x34324241; // ABGR8888
    pub const DRM_FORMAT_XBGR8888: u32 = 0x34324258; // XBGR8888
    pub const DRM_FORMAT_RGBA8888: u32 = 0x34324152; // RGBA8888
    pub const DRM_FORMAT_RGBX8888: u32 = 0x34325852; // RGBX8888
    pub const DRM_FORMAT_BGRA8888: u32 = 0x34324142; // BGRA8888
    pub const DRM_FORMAT_BGRX8888: u32 = 0x34325842; // BGRX8888
    pub const DRM_FORMAT_NV12: u32 = 0x3231564e;     // NV12 (YUV 4:2:0)
    pub const DRM_FORMAT_YUV420: u32 = 0x32315559;   // YUV420

    /// Linear modifier (no tiling)
    pub const DRM_FORMAT_MOD_LINEAR: u64 = 0;
    /// Invalid modifier
    pub const DRM_FORMAT_MOD_INVALID: u64 = 0x00ffffffffffffff;
}

/// Convert DRM fourcc to Vulkan format
#[cfg(all(target_os = "linux", feature = "ash"))]
pub fn drm_fourcc_to_vk_format(fourcc: u32) -> Option<vk::Format> {
    match fourcc {
        drm_fourcc::DRM_FORMAT_ARGB8888 | drm_fourcc::DRM_FORMAT_XRGB8888 => {
            Some(vk::Format::B8G8R8A8_UNORM)
        }
        drm_fourcc::DRM_FORMAT_ABGR8888 | drm_fourcc::DRM_FORMAT_XBGR8888 => {
            Some(vk::Format::R8G8B8A8_UNORM)
        }
        drm_fourcc::DRM_FORMAT_RGBA8888 | drm_fourcc::DRM_FORMAT_RGBX8888 => {
            Some(vk::Format::R8G8B8A8_UNORM)
        }
        drm_fourcc::DRM_FORMAT_BGRA8888 | drm_fourcc::DRM_FORMAT_BGRX8888 => {
            Some(vk::Format::B8G8R8A8_UNORM)
        }
        _ => None,
    }
}

/// Convert DRM fourcc to wgpu format
pub fn drm_fourcc_to_wgpu_format(fourcc: u32) -> Option<wgpu::TextureFormat> {
    match fourcc {
        drm_fourcc::DRM_FORMAT_ARGB8888 | drm_fourcc::DRM_FORMAT_XRGB8888 |
        drm_fourcc::DRM_FORMAT_BGRA8888 | drm_fourcc::DRM_FORMAT_BGRX8888 => {
            Some(wgpu::TextureFormat::Bgra8Unorm)
        }
        drm_fourcc::DRM_FORMAT_ABGR8888 | drm_fourcc::DRM_FORMAT_XBGR8888 |
        drm_fourcc::DRM_FORMAT_RGBA8888 | drm_fourcc::DRM_FORMAT_RGBX8888 => {
            Some(wgpu::TextureFormat::Rgba8Unorm)
        }
        _ => None,
    }
}

/// DMA-BUF import parameters
#[cfg(target_os = "linux")]
#[derive(Debug, Clone)]
pub struct DmaBufImportParams {
    /// DMA-BUF file descriptor
    pub fd: RawFd,
    /// Width in pixels
    pub width: u32,
    /// Height in pixels
    pub height: u32,
    /// Stride (bytes per row)
    pub stride: u32,
    /// DRM fourcc format
    pub fourcc: u32,
    /// DRM modifier
    pub modifier: u64,
    /// Offset within the DMA-BUF
    pub offset: u32,
}

/// Vulkan DMA-BUF importer
///
/// Manages Vulkan resources for importing DMA-BUF as textures.
/// This is a zero-copy path that avoids CPU-GPU transfers.
#[cfg(all(target_os = "linux", feature = "ash"))]
pub struct VulkanDmaBufImporter {
    /// Whether the required extensions are available
    supported: bool,
}

#[cfg(all(target_os = "linux", feature = "ash"))]
impl VulkanDmaBufImporter {
    /// Create a new importer, checking for required extensions
    pub fn new() -> Self {
        // For now, we'll check extension support when importing
        // Full validation would require access to the Vulkan instance
        Self { supported: true }
    }

    /// Check if DMA-BUF import is supported
    pub fn is_supported(&self) -> bool {
        self.supported
    }

    /// Import a DMA-BUF as a wgpu texture
    ///
    /// This is a complex operation that:
    /// 1. Creates a Vulkan image with external memory
    /// 2. Imports the DMA-BUF fd
    /// 3. Wraps as wgpu texture
    ///
    /// Currently returns None as wgpu doesn't expose the needed HAL access.
    /// A working implementation would require either:
    /// - wgpu supporting external memory import directly
    /// - Using raw Vulkan alongside wgpu (complex ownership)
    pub fn import_dmabuf(
        &self,
        _device: &wgpu::Device,
        _queue: &wgpu::Queue,
        _params: &DmaBufImportParams,
    ) -> Option<wgpu::Texture> {
        // TODO: Full implementation requires HAL access
        //
        // The wgpu crate intentionally doesn't expose raw Vulkan handles
        // for safety. To implement this properly, we would need:
        //
        // 1. Use wgpu's HAL layer directly (wgpu_hal crate)
        // 2. Create texture via HAL with external memory
        // 3. Use device.create_texture_from_hal()
        //
        // For now, we log and fall back to CPU copy path

        log::debug!(
            "VulkanDmaBufImporter: DMA-BUF import not yet implemented (would import fd={})",
            _params.fd
        );

        None
    }
}

#[cfg(all(target_os = "linux", feature = "ash"))]
impl Default for VulkanDmaBufImporter {
    fn default() -> Self {
        Self::new()
    }
}

/// Fallback: read DMA-BUF contents via mmap and create texture via CPU copy
///
/// This is slower than zero-copy but works without special Vulkan extensions.
#[cfg(target_os = "linux")]
pub fn import_dmabuf_via_mmap(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    params: &DmaBufImportParams,
) -> Option<wgpu::Texture> {
    use std::os::unix::io::FromRawFd;
    use std::fs::File;
    use std::io::Read;

    let wgpu_format = drm_fourcc_to_wgpu_format(params.fourcc)?;

    // Calculate expected size
    let expected_size = (params.stride * params.height) as usize;

    // Dup the fd so we don't close the original
    let fd_dup = unsafe { libc::dup(params.fd) };
    if fd_dup < 0 {
        log::warn!("import_dmabuf_via_mmap: failed to dup fd");
        return None;
    }

    // mmap the DMA-BUF
    let data = unsafe {
        let ptr = libc::mmap(
            std::ptr::null_mut(),
            expected_size,
            libc::PROT_READ,
            libc::MAP_SHARED,
            fd_dup,
            params.offset as i64,
        );

        if ptr == libc::MAP_FAILED {
            libc::close(fd_dup);
            log::warn!("import_dmabuf_via_mmap: mmap failed");
            return None;
        }

        // Copy data out of mmap
        let slice = std::slice::from_raw_parts(ptr as *const u8, expected_size);
        let data = slice.to_vec();

        // Unmap and close
        libc::munmap(ptr, expected_size);
        libc::close(fd_dup);

        data
    };

    // Create texture
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("DMA-BUF mmap texture"),
        size: wgpu::Extent3d {
            width: params.width,
            height: params.height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu_format,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });

    // Upload data
    queue.write_texture(
        wgpu::ImageCopyTexture {
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        &data,
        wgpu::ImageDataLayout {
            offset: 0,
            bytes_per_row: Some(params.stride),
            rows_per_image: Some(params.height),
        },
        wgpu::Extent3d {
            width: params.width,
            height: params.height,
            depth_or_array_layers: 1,
        },
    );

    log::debug!(
        "import_dmabuf_via_mmap: created {}x{} texture via mmap (not zero-copy)",
        params.width,
        params.height
    );

    Some(texture)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drm_fourcc_to_wgpu() {
        assert_eq!(
            drm_fourcc_to_wgpu_format(drm_fourcc::DRM_FORMAT_ARGB8888),
            Some(wgpu::TextureFormat::Bgra8Unorm)
        );
        assert_eq!(
            drm_fourcc_to_wgpu_format(drm_fourcc::DRM_FORMAT_RGBA8888),
            Some(wgpu::TextureFormat::Rgba8Unorm)
        );
    }
}
