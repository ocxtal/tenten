mod axis;
mod color;
mod hit;
mod layout;
mod plane;
mod plot;
mod sequence;

use anyhow::Result;
pub use axis::{AxisAppearance, LengthScale};
pub use color::{AnnotationColorMap, ColorScale, DensityColorMap};
pub use hit::{DotPlotHit, PlotHitMap, SequencePosition};
pub use plane::DotPlane;
pub use plot::{DotPlot, DotPlotAppearance};
use plotters::prelude::*;
pub use plotters::prelude::{RGBColor, TextStyle};
use plotters_backend::DrawingBackend;
pub use sequence::{RangeFormat, SequenceRange, load_sequence_range};

use crate::dotplot::layout::{Layout, StructuredDrawingArea, build_plot_layout};

#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub struct PlotImage {
    pub width: u32,
    pub height: u32,
    pub rgba: Vec<u8>,
}

fn draw_plot_to_areas<DB>(areas: &StructuredDrawingArea<DB>, dotplot: &DotPlot) -> Result<()>
where
    DB: DrawingBackend + IntoDrawingArea,
    <DB as DrawingBackend>::ErrorType: 'static,
{
    let appearance = dotplot.appearance();
    let scale_appearance = AxisAppearance {
        fit_in_box: false,
        ..appearance.x_axis_appearance.clone()
    };
    let length_scale = LengthScale::new(
        dotplot.base_per_pixel(),
        2 * appearance.desired_tick_pitch as usize,
        &scale_appearance,
    );
    let color_scale = ColorScale::new(dotplot.color_map(), 250, &scale_appearance);

    if let Some(area) = areas.get_area("dotplot") {
        area.draw(dotplot)?;
    }
    if let Some(area) = areas.get_area("length_scale") {
        area.draw(&length_scale)?;
    }
    if let Some(area) = areas.get_area("color_scale") {
        area.draw(&color_scale)?;
    }
    Ok(())
}

fn draw_with_backend<DB>(backend: DB, layout: &Layout, dotplot: &DotPlot) -> Result<()>
where
    DB: DrawingBackend + IntoDrawingArea,
    <DB as DrawingBackend>::ErrorType: 'static,
{
    let areas = StructuredDrawingArea::from_backend_and_layout(backend, layout)?;
    draw_plot_to_areas(&areas, dotplot)?;
    areas.present()?;
    Ok(())
}

pub fn render_rgba(dotplot: &DotPlot, hide_scale: bool) -> Result<PlotImage> {
    let layout = build_plot_layout(dotplot, hide_scale);
    let (width, height) = layout.0.get_dim();
    let mut rgb = vec![0; width as usize * height as usize * 3];
    {
        let backend = BitMapBackend::with_buffer(&mut rgb, (width, height));
        draw_with_backend(backend, &layout, dotplot)?;
    }

    let mut rgba = Vec::with_capacity(width as usize * height as usize * 4);
    for pixel in rgb.chunks_exact(3) {
        rgba.extend_from_slice(&[pixel[0], pixel[1], pixel[2], 0xff]);
    }

    Ok(PlotImage { width, height, rgba })
}

pub fn plot(name: &str, dotplot: &DotPlot, hide_scale: bool) -> Result<()> {
    let layout = build_plot_layout(dotplot, hide_scale);
    if name.ends_with(".png") {
        let backend = BitMapBackend::new(name, layout.0.get_dim());
        draw_with_backend(backend, &layout, dotplot)?;
    } else {
        let backend = SVGBackend::new(name, layout.0.get_dim());
        draw_with_backend(backend, &layout, dotplot)?;
    }
    Ok(())
}
