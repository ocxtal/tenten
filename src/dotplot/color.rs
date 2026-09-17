use crate::dotplot::axis::{Axis, AxisAppearance, Tick};
use crate::dotplot::layout::{Layout, LayoutElem, RectAnchor};
use crate::dotplot::{Density, Direction, LINE_DENSITY_SCALE};
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::{BackendStyle, DrawingErrorKind};
use regex::Regex;
use std::collections::HashMap;

#[derive(Copy, Clone, Debug)]
pub struct DensityColorMap {
    pub palette: [RGBColor; 2],
    pub max_density: f64,
    pub min_density: f64,
}

impl DensityColorMap {
    pub(crate) fn to_picker(self, density: Density) -> DensityColorPicker {
        let count_per_density = match density {
            Density::Pixel => 1.0,
            Density::Line { .. } => LINE_DENSITY_SCALE,
        };
        DensityColorPicker {
            palette: self.palette,
            offset: (self.min_density * count_per_density).log2(),
            scale: 1.0 / (self.max_density.log2() - self.min_density.log2()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct DensityColorPicker {
    palette: [RGBColor; 2],
    offset: f64,
    scale: f64,
}

impl DensityColorPicker {
    pub fn get_color(&self, palette_index: usize, count: f64) -> RGBAColor {
        let intensity = self.scale * (count.log2() - self.offset);
        self.palette[palette_index].mix(intensity.clamp(0.0, 1.0))
    }
}

#[derive(Clone, Debug)]
pub struct AnnotationColorMap {
    pub palette: HashMap<String, RGBColor>,
    pub default: RGBColor,
    pub alpha: f64,
    pub prefix_match: bool,
    pub exact_match: bool,
}

impl AnnotationColorMap {
    pub fn add_annotation(&mut self, name: String, color: RGBColor) {
        self.palette.insert(name, color);
    }

    pub(crate) fn to_picker(&self) -> AnnotationColorPicker {
        let mut v = Vec::new();
        for (key, color) in self.palette.iter() {
            let key = if self.exact_match {
                format!("^{}$", key)
            } else if self.prefix_match {
                format!("^{}", key)
            } else {
                key.to_string()
            };
            let re = Regex::new(&key).unwrap();
            let color = color.mix(self.alpha);
            v.push((re, color));
        }
        let default = self.default.mix(self.alpha);
        AnnotationColorPicker { v, default }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct AnnotationColorPicker {
    v: Vec<(Regex, RGBAColor)>,
    default: RGBAColor,
}

impl AnnotationColorPicker {
    pub fn get_color(&self, name: &str) -> RGBAColor {
        self.v
            .iter()
            .filter_map(|(regex, color)| {
                if let Some(m) = regex.find(name) {
                    let len = m.end() - m.start();
                    Some((len, *color))
                } else {
                    None
                }
            })
            .max_by_key(|(len, _)| *len)
            .unwrap_or((0, self.default))
            .1
    }

    pub fn default_color(&self) -> RGBAColor {
        self.default
    }
}

#[derive(Clone)]
pub struct ColorScale<'a> {
    len: u32,
    picker: DensityColorPicker,
    min_density: f64,
    density_ratio: f64,
    count_per_density: f64,
    unit: &'static str,
    axis: Axis,
    app: &'a AxisAppearance<'a>,
}

impl<'a> ColorScale<'a> {
    pub fn new(color_map: &'_ DensityColorMap, density: Density, desired_length: usize, appearance: &'a AxisAppearance) -> ColorScale<'a> {
        let desired_length = desired_length as u32;
        let axis = Axis::new(1, desired_length / 4);
        let len = axis.label_period * axis.pitch_in_bases;
        let (min_density, count_per_density, unit) = match density {
            Density::Pixel => (1.0, 1.0, "/kbp^2"),
            Density::Line { .. } => (color_map.min_density, LINE_DENSITY_SCALE, "/kbp"),
        };
        ColorScale {
            len,
            picker: color_map.to_picker(density),
            min_density,
            density_ratio: color_map.max_density / min_density,
            count_per_density,
            unit,
            axis,
            app: appearance,
        }
    }

    pub fn get_dim(&self) -> (u32, u32) {
        let w = self.len + 1;
        let h = self.app.large_tick_length + self.app.axis_thickness + self.app.label_setback + self.app.label_style.font.get_size() as u32;
        (w, h)
    }

    fn density_at(&self, fraction: f64) -> f64 {
        self.min_density * self.density_ratio.powf(fraction)
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a ColorScale<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
    }
}

impl<DB> Drawable<DB> for ColorScale<'_>
where
    DB: DrawingBackend,
{
    fn draw<I>(&self, pos: I, backend: &mut DB, _: (u32, u32)) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        I: Iterator<Item = (i32, i32)>,
    {
        let mut pos = pos;
        let pos = pos.next().unwrap();
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);

        // first build ticks (to determine actual width)
        let ticks = Tick::build_vec(
            (0, 0),
            0..self.len as isize,
            Direction::Right,
            Direction::Down,
            &self.axis,
            self.app,
            |i, _| format!("{:.1}", self.density_at(i as f64 / self.len as f64)),
        );
        let len = ticks.last().unwrap().tick_start.0;
        let width = len as u32 + 1;

        // compose layout using the width determined above
        let layout = Layout(LayoutElem::Vertical(vec![
            LayoutElem::Rect {
                id: Some("fw".to_string()),
                width,
                height: self.app.large_tick_length,
            },
            LayoutElem::Rect {
                id: Some("axis".to_string()),
                width,
                height: self.app.axis_thickness,
            },
            LayoutElem::Rect {
                id: Some("rv".to_string()),
                width,
                height: self.app.large_tick_length,
            },
        ]));

        // draw color scale
        let range = layout.get_range("fw").unwrap();
        let (fw_x, fw_y) = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft);
        let fw_shift = |(x, y): (i32, i32)| shift((fw_x + x + 1, fw_y + y));

        let range = layout.get_range("rv").unwrap();
        let (rv_x, rv_y) = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft);
        let rv_shift = |(x, y): (i32, i32)| shift((rv_x + x + 1, rv_y + y));

        let height = self.app.large_tick_length as i32;
        for i in 0..len {
            let cnt = (self.density_at(i as f64 / len as f64) * self.count_per_density).floor();
            let cf = self.picker.get_color(0, cnt).color();
            backend.draw_rect(fw_shift((i, 0)), fw_shift((i + 1, height)), &cf, true)?;

            let cr = self.picker.get_color(1, cnt).color();
            backend.draw_rect(rv_shift((i, 0)), rv_shift((i + 1, height)), &cr, true)?;
        }

        // draw ticks
        let mut ticks = ticks;
        let adj = self.app.large_tick_length as i32 + self.app.axis_thickness as i32;
        if let Some(tick) = ticks.first_mut() {
            tick.tick_start.1 -= adj;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label = format!("{}{}", &tick.label, self.unit);
            tick.tick_start.1 -= adj;
        }

        let range = layout.get_range("rv").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft));
        for (i, tick) in ticks.iter().enumerate() {
            let show_label = if i as u32 == self.axis.label_period {
                true
            } else if i as u32 == self.axis.label_period - 1 {
                false
            } else {
                i % 2 == 0
            };
            let tick = Tick {
                show_label,
                ..tick.clone()
            };
            tick.draw(std::iter::once(pos), backend, (0, 0))?;
        }

        // draw axis
        let range = layout.get_range("axis").unwrap();
        backend.draw_rect(
            shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft)),
            shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomRight)),
            &BLACK.color(),
            true,
        )?;
        Ok(())
    }
}
