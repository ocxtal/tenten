mod axis;
mod color;
mod layout;
mod plane;
mod plot;
mod sequence;

use anyhow::Result;
pub use axis::{AxisAppearance, LengthScale};
pub use color::{AnnotationColorMap, ColorScale, DensityColorMap};
pub use plane::DotPlane;
pub use plot::{DotPlot, DotPlotAppearance};
pub use plotters::prelude::{RGBColor, TextStyle};
pub use sequence::{RangeFormat, SequenceRange, load_sequence_range};

use crate::dotplot::layout::{Layout, LayoutElem, LayoutMargin, StructuredDrawingArea};

#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub fn plot(name: &str, dotplot: &DotPlot, hide_scale: bool) -> Result<()> {
    let appearance = dotplot.appearance();
    let scale_appearance = AxisAppearance {
        fit_in_box: false,
        ..appearance.x_axis_appearance.clone()
    };
    let length_scale = LengthScale::new(dotplot.base_per_pixel(), appearance.desired_tick_pitch as usize, &scale_appearance);
    let color_scale = ColorScale::new(dotplot.color_map(), 200, &scale_appearance);

    let mut center = Vec::new();
    if !hide_scale {
        center.push(LayoutElem::Horizontal(vec![
            LayoutElem::Rect {
                id: None,
                width: 50,
                height: 30,
            },
            LayoutElem::Margined {
                margin: LayoutMargin::new(0, 50, 10, 10),
                center: Box::new(LayoutElem::Rect {
                    id: Some("length_scale".to_string()),
                    width: length_scale.get_dim().0,
                    height: length_scale.get_dim().1,
                }),
            },
            LayoutElem::Margined {
                margin: LayoutMargin::new(0, 50, 10, 10),
                center: Box::new(LayoutElem::Rect {
                    id: Some("color_scale".to_string()),
                    width: color_scale.get_dim().0,
                    height: color_scale.get_dim().1,
                }),
            },
        ]));
    }
    center.push(LayoutElem::Rect {
        id: Some("dotplot".to_string()),
        width: dotplot.get_dim().0,
        height: dotplot.get_dim().1,
    });

    let layout = Layout(LayoutElem::Margined {
        margin: LayoutMargin::uniform(20),
        center: Box::new(LayoutElem::Vertical(center)),
    });
    let areas = StructuredDrawingArea::from_layout(&layout, name)?;

    if let Some(area) = areas.get_area("dotplot") {
        area.draw(dotplot)?;
    }
    if let Some(area) = areas.get_area("length_scale") {
        area.draw(&length_scale)?;
    }
    if let Some(area) = areas.get_area("color_scale") {
        area.draw(&color_scale)?;
    }
    areas.present()?;
    Ok(())
}
