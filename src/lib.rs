pub mod axis;
pub mod color;
pub mod dotplane;
pub mod dotplot;
pub mod layout;
pub mod parser;
pub mod sequence;

use anyhow::Result;
pub use axis::{AxisAppearance, LengthScale};
pub use color::{ColorScale, DensityColorMap};
pub use dotplane::DotPlane;
pub use dotplot::{DotPlot, DotPlotAppearance};
pub use plotters::prelude::{RGBColor, TextStyle};
pub use sequence::{RangeFormat, SequenceRange, load_sequence_range};

use crate::axis::Axis;
use crate::layout::{Layout, LayoutElem, LayoutMargin, StructuredDrawingArea};
use plotters::prelude::*;

#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

pub fn plot(name: &str, dotplot: &DotPlot) -> Result<()> {
    let appearance = dotplot.appearance();
    let scale_appearance = AxisAppearance {
        fit_in_box: false,
        ..appearance.x_axis_appearance.clone()
    };
    let length_scale = LengthScale::new(dotplot.base_per_pixel(), appearance.desired_tick_pitch as usize, &scale_appearance);
    let color_scale = ColorScale::new(dotplot.color_map(), 200, &scale_appearance);

    eprintln!(
        "dim: {:?}, {:?}, {:?}",
        dotplot.get_dim(),
        length_scale.get_dim(),
        color_scale.get_dim()
    );

    let layout = Layout(LayoutElem::Margined {
        margin: LayoutMargin::uniform(20),
        center: Box::new(LayoutElem::Vertical(vec![
            LayoutElem::Horizontal(vec![
                LayoutElem::Rect {
                    id: None,
                    width: 50,
                    height: 30,
                },
                LayoutElem::Margined {
                    margin: LayoutMargin::new(0, 30, 10, 10),
                    center: Box::new(LayoutElem::Rect {
                        id: Some("length_scale".to_string()),
                        width: length_scale.get_dim().0,
                        height: length_scale.get_dim().1,
                    }),
                },
                LayoutElem::Margined {
                    margin: LayoutMargin::new(0, 30, 10, 10),
                    center: Box::new(LayoutElem::Rect {
                        id: Some("color_scale".to_string()),
                        width: color_scale.get_dim().0,
                        height: color_scale.get_dim().1,
                    }),
                },
            ]),
            LayoutElem::Rect {
                id: Some("dotplot".to_string()),
                width: dotplot.get_dim().0,
                height: dotplot.get_dim().1,
            },
        ])),
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
