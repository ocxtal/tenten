use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use plotters_backend::DrawingErrorKind;
use std::ops::Range;

#[derive(Copy, Clone, Debug)]
pub struct Axis {
    base_per_pixel: u32,
    label_period: u32,
    pitch_in_bases: u32,
    pixels_per_pitch: f64,
    subunit: u32,
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
pub struct AxisAppearance<'a> {
    pub large_tick_length: u32,
    pub small_tick_length: u32,
    pub label_setback: u32,
    pub label_style: TextStyle<'a>,
    pub fit_in_box: bool,
}

#[derive(Clone)]
struct Tick<'a> {
    tick_start: (i32, i32),
    tick_end: (i32, i32),
    label_pos: (i32, i32),
    label_anchor: Pos,
    label_style: TextStyle<'a>,
    label: String,
    show_label: bool,
}

impl<'a> Tick<'a> {
    fn build_vec<F>(
        root: (i32, i32),
        range: Range<usize>,
        axis_direction: Direction,
        tick_direction: Direction,
        axis: &'_ Axis,
        app: &'a AxisAppearance,
        label_formatter: F,
    ) -> Vec<Tick<'a>>
    where
        F: Fn(u32, u32) -> String,
    {
        let range = range.start as u32..range.end as u32;
        let base_to_pixel = axis.pixels_per_pitch / axis.pitch_in_bases as f64;
        let build = |i: u32, is_large: bool, show_label: bool| {
            Self::build(
                root,
                ((i - range.start) as f64 * base_to_pixel) as i32,
                tick_direction,
                axis_direction,
                (i == range.start, i == range.end),
                is_large,
                app,
                label_formatter(i, axis.subunit),
                show_label,
            )
        };

        let mut labels = Vec::new();
        labels.push(build(range.start, true, true));

        let mut i = range.start;
        loop {
            let next_n = (i + 1).div_ceil(axis.pitch_in_bases);
            i = next_n * axis.pitch_in_bases;
            if i >= range.end {
                break;
            }

            let thresh = axis.pitch_in_bases * 3;
            let too_close_to_end = i <= range.start + thresh || i + thresh >= range.end;
            let is_large = next_n % axis.label_period == 0;
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
