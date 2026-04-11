use anyhow::{Context, Result};
use pixels::{Pixels, SurfaceTexture};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use std::sync::Arc;
use tenten::{DotPlotHit, PlotImage, TextStyle};
use winit::application::ApplicationHandler;
use winit::dpi::{PhysicalPosition, PhysicalSize};
use winit::event::{ElementState, MouseButton, MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowId};

const LINE_DELTA_IN_PIXELS: f64 = 40.0;
const TOOLTIP_PADDING: u32 = 2;
const TOOLTIP_BORDER: u32 = 1;

pub fn show(image: PlotImage, title: &str, tooltip_style: TextStyle<'static>) -> Result<()> {
    let event_loop = EventLoop::new().context("failed to create event loop")?;
    let mut app = ViewerApp::new(image, title, tooltip_style);
    event_loop.run_app(&mut app).context("window event loop failed")
}

struct Tooltip {
    image_pos: (u32, u32),
    text: String,
}

struct ViewerApp {
    image: PlotImage,
    title: String,
    tooltip_style: TextStyle<'static>,
    window: Option<Arc<Window>>,
    pixels: Option<Pixels<'static>>,
    scroll_x: f64,
    scroll_y: f64,
    cursor_pos: Option<PhysicalPosition<f64>>,
    tooltip: Option<Tooltip>,
}

impl ViewerApp {
    fn new(image: PlotImage, title: &str, tooltip_style: TextStyle<'static>) -> Self {
        Self {
            image,
            title: title.to_string(),
            tooltip_style: tooltip_style.pos(Pos::new(HPos::Left, VPos::Top)),
            window: None,
            pixels: None,
            scroll_x: 0.0,
            scroll_y: 0.0,
            cursor_pos: None,
            tooltip: None,
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

    fn format_tooltip(hit: &DotPlotHit) -> Option<String> {
        match (&hit.target, &hit.query) {
            (Some(target), Some(query)) => Some(format!("({}, {})", target.pos, query.pos)),
            (Some(target), None) => Some(format!("({})", target.pos)),
            (None, Some(query)) => Some(format!("({})", query.pos)),
            (None, None) => None,
        }
    }

    fn update_tooltip(&mut self) {
        self.tooltip = None;

        let Some(cursor_pos) = self.cursor_pos else {
            return;
        };
        let image_x = cursor_pos.x + self.scroll_x.round();
        let image_y = cursor_pos.y + self.scroll_y.round();
        if image_x < 0.0 || image_y < 0.0 || image_x >= self.image.width as f64 || image_y >= self.image.height as f64 {
            return;
        }

        let image_pos = (image_x.floor() as u32, image_y.floor() as u32);
        if let Some(text) = self
            .image
            .hit_test(image_pos.0, image_pos.1)
            .and_then(|hit| Self::format_tooltip(&hit))
        {
            self.tooltip = Some(Tooltip { image_pos, text });
        }
    }

    fn draw_tooltip_to_frame(
        frame: &mut [u8],
        frame_width: usize,
        frame_height: usize,
        scroll_x: usize,
        scroll_y: usize,
        tooltip: &Tooltip,
        style: &TextStyle<'static>,
    ) -> Result<()> {
        let (text_width, text_height) = {
            let mut rgb = vec![255; 3];
            let area = BitMapBackend::with_buffer(&mut rgb, (1, 1)).into_drawing_area();
            area.estimate_text_size(&tooltip.text, style)?
        };
        let width = text_width + 2 * (TOOLTIP_PADDING + TOOLTIP_BORDER);
        let height = text_height + 2 * (TOOLTIP_PADDING + TOOLTIP_BORDER);
        let mut rgb = vec![255; width as usize * height as usize * 3];
        {
            let area = BitMapBackend::with_buffer(&mut rgb, (width, height)).into_drawing_area();
            area.fill(&WHITE)?;
            area.draw(&Rectangle::new(
                [(0, 0), (width as i32 - 1, height as i32 - 1)],
                ShapeStyle::from(&BLACK).stroke_width(1),
            ))?;
            area.draw_text(
                &tooltip.text,
                style,
                ((TOOLTIP_BORDER + TOOLTIP_PADDING) as i32, (TOOLTIP_BORDER + TOOLTIP_PADDING) as i32),
            )?;
        }

        let dst_x0 = tooltip.image_pos.0 as i32 - scroll_x as i32;
        let dst_y0 = tooltip.image_pos.1 as i32 - scroll_y as i32;
        for y in 0..height as i32 {
            let dst_y = dst_y0 + y;
            if !(0..frame_height as i32).contains(&dst_y) {
                continue;
            }
            for x in 0..width as i32 {
                let dst_x = dst_x0 + x;
                if !(0..frame_width as i32).contains(&dst_x) {
                    continue;
                }
                let src = (y as usize * width as usize + x as usize) * 3;
                let dst = (dst_y as usize * frame_width + dst_x as usize) * 4;
                frame[dst..dst + 4].copy_from_slice(&[rgb[src], rgb[src + 1], rgb[src + 2], 255]);
            }
        }

        Ok(())
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

        if let Some(tooltip) = &self.tooltip {
            Self::draw_tooltip_to_frame(frame, frame_width, frame_height, scroll_x, scroll_y, tooltip, &self.tooltip_style)
                .context("failed to draw tooltip")?;
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
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor_pos = Some(position);
            }
            WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: MouseButton::Left,
                ..
            } => {
                self.update_tooltip();
                self.window.as_ref().unwrap().request_redraw();
            }
            WindowEvent::KeyboardInput { event, .. } => {
                if event.state == ElementState::Pressed && event.physical_key == PhysicalKey::Code(KeyCode::Escape) {
                    self.tooltip = None;
                    self.window.as_ref().unwrap().request_redraw();
                }
            }
            WindowEvent::RedrawRequested => {
                self.draw().unwrap();
            }
            _ => {}
        }
    }
}
