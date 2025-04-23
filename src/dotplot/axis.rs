use crate::dotplot::Direction;
use crate::dotplot::layout::{Layout, LayoutElem, RectAnchor};
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::ops::Range;

#[derive(Clone)]
pub struct AxisAppearance<'a> {
    pub axis_thickness: u32,
    pub large_tick_length: u32,
    pub small_tick_length: u32,
    pub label_setback: u32,
    pub label_style: TextStyle<'a>,
    pub fit_in_box: bool,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Axis {
    pub base_per_pixel: u32,
    pub label_period: u32,
    pub pitch_in_bases: u32,
    pub pixels_per_pitch: f64,
    pub subunit: u32,
}

impl Axis {
    pub fn new(base_per_pixel: u32, desired_pitch_in_pixels: u32) -> Axis {
        let unit_bases = (desired_pitch_in_pixels as f64 * base_per_pixel as f64).log(10.0);
        let (f, c) = (unit_bases.fract(), unit_bases.floor());
        assert!(f <= 1.0 && c >= 1.0);

        let (label_period, pitch_in_bases) = if f < 2.5f64.log(10.0) {
            (5, 10u64.pow(c as u32))
        } else if f < 5.0f64.log(10.0) {
            (4, 10u64.pow(c as u32 + 1) / 4)
        } else {
            (5, 10u64.pow(c as u32 + 1) / 2)
        };
        let subunit = 10u32.pow(pitch_in_bases.ilog10() / 3 * 3);
        Axis {
            base_per_pixel,
            label_period,
            pitch_in_bases: pitch_in_bases as u32,
            pixels_per_pitch: pitch_in_bases as f64 / base_per_pixel as f64,
            subunit,
        }
    }

    pub fn get_subunit_text(&self) -> &'static str {
        ["", "k", "M", "G", "T", "P", "E", "Z", "Y"][self.subunit.ilog(10) as usize / 3]
    }
}

#[derive(Clone)]
pub(crate) struct Tick<'a> {
    pub tick_start: (i32, i32),
    pub tick_end: (i32, i32),
    pub label_pos: (i32, i32),
    pub label_anchor: Pos,
    pub label_style: TextStyle<'a>,
    pub label: String,
    pub show_label: bool,
}

impl<'a> Tick<'a> {
    pub fn build_vec<F>(
        root: (i32, i32),
        range: Range<isize>,
        axis_direction: Direction,
        tick_direction: Direction,
        axis: &'_ Axis,
        app: &'a AxisAppearance,
        label_formatter: F,
    ) -> Vec<Tick<'a>>
    where
        F: Fn(isize, usize) -> String,
    {
        let base_to_pixel = axis.pixels_per_pitch / axis.pitch_in_bases as f64;
        let build = |i: isize, is_large: bool, show_label: bool| {
            Self::build(
                root,
                ((i - range.start) as f64 * base_to_pixel) as i32,
                tick_direction,
                axis_direction,
                (i == range.start, i == range.end),
                is_large,
                app,
                label_formatter(i, axis.subunit as usize),
                show_label,
            )
        };

        let mut labels = Vec::new();
        labels.push(build(range.start, true, true));

        let pitch = axis.pitch_in_bases as isize;
        let label_period = axis.label_period as isize;
        let hide_thresh = pitch * 3;
        let mut i = range.start;
        loop {
            let next_n = (i + pitch).div_euclid(pitch);
            i = next_n * pitch;
            if i >= range.end {
                break;
            }

            let too_close_to_end = i <= range.start + hide_thresh || i + hide_thresh >= range.end;
            let is_large = next_n % label_period == 0;
            let show_label = is_large && !too_close_to_end;
            labels.push(build(i, is_large, show_label));
        }
        labels.push(build(range.end, true, true));
        labels
    }

    #[allow(clippy::too_many_arguments)]
    fn build(
        root: (i32, i32),
        pos: i32,
        tick_direction: Direction,
        axis_direction: Direction,
        is_end: (bool, bool),
        is_large: bool,
        app: &'a AxisAppearance,
        label: String,
        show_label: bool,
    ) -> Tick<'a> {
        let pos = if is_end.0 {
            pos - 1
        } else if is_end.1 {
            pos + 1
        } else {
            pos
        };
        let pos = match axis_direction {
            Direction::Up => (root.0, root.1 - pos - 1),
            Direction::Down => (root.0, root.1 + pos),
            Direction::Left => (root.0 - pos - 1, root.1),
            Direction::Right => (root.0 + pos, root.1),
        };
        let len = if is_large { app.large_tick_length } else { app.small_tick_length } as i32;
        let (tick_start, tick_end) = match tick_direction {
            Direction::Up => ((pos.0, pos.1 - len), (pos.0, pos.1 - 1)),
            Direction::Down => ((pos.0, pos.1), (pos.0, pos.1 + len - 1)),
            Direction::Left => ((pos.0 - len, pos.1), (pos.0 - 1, pos.1)),
            Direction::Right => ((pos.0, pos.1), (pos.0 + len - 1, pos.1)),
        };

        let setback = app.label_setback as i32;
        let end_index = match (app.fit_in_box, is_end.1, is_end.0) {
            (false, _, _) => 1,
            (_, false, true) | (_, true, true) => 0,
            (_, false, false) => 1,
            (_, true, false) => 2,
        };
        let x_anchors = [HPos::Left, HPos::Center, HPos::Right];
        let y_anchors = [VPos::Bottom, VPos::Center, VPos::Top];
        let shift = (app.label_style.font.get_size() / 8.0) as i32;
        let shifts = [shift, 0, -shift];
        let (label_pos, label_anchor) = match tick_direction {
            Direction::Up => (
                (pos.0 + shifts[end_index], pos.1 - setback),
                Pos::new(x_anchors[end_index], VPos::Bottom),
            ),
            Direction::Down => (
                (pos.0 + shifts[end_index], pos.1 + setback),
                Pos::new(x_anchors[end_index], VPos::Top),
            ),
            Direction::Left => ((pos.0 - setback, pos.1), Pos::new(HPos::Right, y_anchors[end_index])),
            Direction::Right => ((pos.0 + setback, pos.1), Pos::new(HPos::Left, y_anchors[end_index])),
        };
        Tick {
            tick_start,
            tick_end,
            label_pos,
            label_anchor,
            label_style: app.label_style.clone(),
            label,
            show_label,
        }
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a Tick<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
    }
}

impl<DB> Drawable<DB> for Tick<'_>
where
    DB: DrawingBackend,
{
    fn draw<I>(&self, pos: I, backend: &mut DB, _: (u32, u32)) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        I: Iterator<Item = (i32, i32)>,
    {
        let mut pos = pos;
        let pos = pos.next().unwrap();
        let style = ShapeStyle {
            color: BLACK.into(),
            filled: false,
            stroke_width: 1,
        };
        let start = (pos.0 + self.tick_start.0, pos.1 + self.tick_start.1);
        let end = (pos.0 + self.tick_end.0, pos.1 + self.tick_end.1);
        backend.draw_line(start, end, &style)?;
        if self.show_label {
            let style = &self.label_style.pos(self.label_anchor);
            let pos = (pos.0 + self.label_pos.0, pos.1 + self.label_pos.1);
            backend.draw_text(&self.label, style, pos)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct LengthScale<'a> {
    len: u32,
    axis: Axis,
    app: &'a AxisAppearance<'a>,
}

impl<'a> LengthScale<'a> {
    pub fn new(base_per_pixel: usize, desired_length: usize, axis_appearance: &'a AxisAppearance) -> LengthScale<'a> {
        let base_per_pixel = base_per_pixel as u32;
        let desired_length = desired_length as u32;
        let axis = Axis::new(base_per_pixel, desired_length);
        let len = (axis.label_period * axis.pitch_in_bases).div_ceil(desired_length) * desired_length;
        LengthScale {
            len,
            axis,
            app: axis_appearance,
        }
    }

    pub fn get_dim(&self) -> (u32, u32) {
        let w = self.len.div_ceil(self.axis.base_per_pixel) + 1;
        let h = self.app.large_tick_length + self.app.axis_thickness + self.app.label_setback + self.app.label_style.font.get_size() as u32;
        (w, h)
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a LengthScale<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
    }
}

impl<DB> Drawable<DB> for LengthScale<'_>
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

        // ticks
        let ticks = Tick::build_vec(
            (0, 0),
            0..self.len as isize,
            Direction::Right,
            Direction::Down,
            &self.axis,
            self.app,
            |i, subunit| format!("{:.1}", i as f64 / subunit as f64),
        );
        let width = ticks.last().unwrap().tick_start.0 as u32 + 1;

        // compose layout using the width determined above
        let layout = Layout(LayoutElem::Vertical(vec![
            LayoutElem::Rect {
                id: Some("up_ticks".to_string()),
                width,
                height: self.app.large_tick_length,
            },
            LayoutElem::Rect {
                id: Some("axis".to_string()),
                width,
                height: self.app.axis_thickness,
            },
            LayoutElem::Rect {
                id: Some("down_ticks".to_string()),
                width,
                height: self.app.large_tick_length,
            },
        ]));

        // extend the first and last ticks upward
        let mut ticks = ticks;
        let adj = self.app.large_tick_length as i32 + self.app.axis_thickness as i32;
        if let Some(tick) = ticks.first_mut() {
            tick.tick_start.1 -= adj;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label = format!("{} {}bp", &tick.label, self.axis.get_subunit_text());
            tick.tick_start.1 -= adj;
        }

        let range = layout.get_range("down_ticks").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft));
        for tick in &ticks {
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
