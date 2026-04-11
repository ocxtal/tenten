use anyhow::{Context, Result};
use pixels::{Pixels, SurfaceTexture};
use std::sync::Arc;
use tenten::PlotImage;
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::{MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};

const LINE_DELTA_IN_PIXELS: f64 = 40.0;

pub fn show(image: PlotImage, title: &str) -> Result<()> {
    let event_loop = EventLoop::new().context("failed to create event loop")?;
    let mut app = ViewerApp::new(image, title);
    event_loop.run_app(&mut app).context("window event loop failed")
}

struct ViewerApp {
    image: PlotImage,
    title: String,
    window: Option<Arc<Window>>,
    pixels: Option<Pixels<'static>>,
    scroll_x: f64,
    scroll_y: f64,
}

impl ViewerApp {
    fn new(image: PlotImage, title: &str) -> Self {
        Self {
            image,
            title: title.to_string(),
            window: None,
            pixels: None,
            scroll_x: 0.0,
            scroll_y: 0.0,
        }
    }

    fn initial_size(event_loop: &ActiveEventLoop, image: &PlotImage) -> PhysicalSize<u32> {
        let monitor_size = event_loop
            .primary_monitor()
            .or_else(|| event_loop.available_monitors().next())
            .map(|monitor| monitor.size());

        let (width, height) = if let Some(monitor_size) = monitor_size {
            (image.width.min(monitor_size.width), image.height.min(monitor_size.height))
        } else {
            (image.width, image.height)
        };
        PhysicalSize::new(width.max(1), height.max(1))
    }

    fn window_size(&self) -> PhysicalSize<u32> {
        self.window.as_ref().unwrap().inner_size()
    }

    fn clamp_scroll(&mut self) {
        let size = self.window_size();
        let max_x = self.image.width.saturating_sub(size.width) as f64;
        let max_y = self.image.height.saturating_sub(size.height) as f64;
        self.scroll_x = self.scroll_x.clamp(0.0, max_x);
        self.scroll_y = self.scroll_y.clamp(0.0, max_y);
    }

    fn scroll(&mut self, delta: MouseScrollDelta) {
        let (dx, dy) = match delta {
            MouseScrollDelta::LineDelta(x, y) => (x as f64 * LINE_DELTA_IN_PIXELS, y as f64 * LINE_DELTA_IN_PIXELS),
            MouseScrollDelta::PixelDelta(pos) => (pos.x, pos.y),
        };
        self.scroll_x -= dx;
        self.scroll_y -= dy;
        self.clamp_scroll();
    }

    fn draw(&mut self) -> Result<()> {
        self.clamp_scroll();
        let size = self.window_size();
        let frame_width = size.width as usize;
        let frame_height = size.height as usize;
        let image_width = self.image.width as usize;
        let image_height = self.image.height as usize;
        let scroll_x = self.scroll_x.round() as usize;
        let scroll_y = self.scroll_y.round() as usize;
        let pixels = self.pixels.as_mut().unwrap();
        let frame = pixels.frame_mut();

        for y in 0..frame_height {
            let src_y = scroll_y + y;
            let row_start = y * frame_width * 4;
            if src_y < image_height {
                let copy_width = (image_width.saturating_sub(scroll_x)).min(frame_width);
                let src_start = (src_y * image_width + scroll_x) * 4;
                let dst_start = row_start;
                let len = copy_width * 4;
                frame[dst_start..dst_start + len].copy_from_slice(&self.image.rgba[src_start..src_start + len]);
                for pixel in frame[dst_start + len..row_start + frame_width * 4].chunks_exact_mut(4) {
                    pixel.copy_from_slice(&[255, 255, 255, 255]);
                }
            } else {
                for pixel in frame[row_start..row_start + frame_width * 4].chunks_exact_mut(4) {
                    pixel.copy_from_slice(&[255, 255, 255, 255]);
                }
            }
        }

        pixels.render().context("failed to render window")
    }
}

impl ApplicationHandler for ViewerApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let size = Self::initial_size(event_loop, &self.image);
        let attrs = Window::default_attributes().with_title(&self.title).with_inner_size(size);
        let window = Arc::new(event_loop.create_window(attrs).unwrap());
        let inner_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(inner_size.width, inner_size.height, window.clone());
        let pixels = Pixels::new(inner_size.width, inner_size.height, surface_texture).unwrap();
        window.request_redraw();
        self.window = Some(window);
        self.pixels = Some(pixels);
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, window_id: WindowId, event: WindowEvent) {
        if Some(window_id) != self.window.as_ref().map(|window| window.id()) {
            return;
        }

        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(pixels) = self.pixels.as_mut() {
                    pixels.resize_surface(size.width, size.height).unwrap();
                    pixels.resize_buffer(size.width.max(1), size.height.max(1)).unwrap();
                }
                self.clamp_scroll();
                self.window.as_ref().unwrap().request_redraw();
            }
            WindowEvent::MouseWheel { delta, .. } => {
                self.scroll(delta);
                self.window.as_ref().unwrap().request_redraw();
            }
            WindowEvent::RedrawRequested => {
                self.draw().unwrap();
            }
            _ => {}
        }
    }
}
