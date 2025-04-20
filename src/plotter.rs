// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use crate::block::{Block, BlockTile};
use crate::layout::{Layout, LayoutElem, LayoutMargin, RectAnchor, StructuredDrawingArea};
use crate::seq::Seq;
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::ops::Range;

#[derive(Copy, Clone, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug)]
struct Breakpoints {
    v: Vec<u32>, // first element is always zero; ignored when splitting plot plane
}

impl Breakpoints {
    fn from_pixels(pixels: &[u32]) -> Breakpoints {
        assert!(!pixels.is_empty());

        let mut acc = 0;
        let mut v = vec![0];
        for p in pixels {
            acc += p;
            v.push(acc);
        }
        Breakpoints { v }
    }

    fn as_breakpoint_slice(&self) -> &[u32] {
        assert!(self.v.len() >= 2);
        &self.v[1..self.v.len() - 1]
    }

    fn as_anchor_slice(&self) -> &[u32] {
        &self.v
    }

    // fn extend_slice(&mut self, v: &[u32]) {
    //     let offset = *self.v.last().unwrap();
    //     for b in v.iter().skip(1) {
    //         self.v.push(offset + b);
    //     }
    // }

    // fn extend(&mut self, other: &Breakpoints) {
    //     self.extend_slice(&other.v);
    // }

    // fn to_margined(&self, left: Option<u32>, right: Option<u32>) -> Breakpoints {
    //     let mut b = Breakpoints { v: vec![0] };
    //     if let Some(left) = left {
    //         b.extend_slice(&[0, left]);
    //     }
    //     b.extend_slice(&self.v);
    //     if let Some(right) = right {
    //         b.extend_slice(&[0, right]);
    //     }
    //     b
    // }

    // fn to_smashed(&self) -> Breakpoints {
    //     let last = self.v.last().unwrap();
    //     Breakpoints { v: vec![0, *last] }
    // }

    // fn reverse(&mut self) {
    //     dbg!(&self.v);
    //     let mut acc = 0;
    //     let mut v = vec![0];
    //     for w in self.v.windows(2).rev() {
    //         acc += w[1] - w[0];
    //         v.push(acc);
    //     }
    //     self.v = v;
    //     dbg!(&self.v);
    // }

    fn pixels(&self) -> u32 {
        *self.v.last().unwrap()
    }

    fn segments(&self) -> usize {
        self.v.len() - 1
    }
}

#[derive(Copy, Clone, Debug)]
pub struct ColorMap {
    pub palette: [RGBColor; 2],
    pub max_density: f64,
    pub min_density: f64,
}

impl ColorMap {
    fn to_picker(&self, base_per_pixel: f64) -> ColorPicker {
        let expansion = (1000.0 / base_per_pixel).powf(2.0);
        let max_count = self.max_density * expansion;
        let min_count = self.min_density * expansion;
        ColorPicker {
            palette: self.palette,
            expansion,
            offset: min_count.log2(),
            scale: 1.0 / (max_count.log2() - min_count.log2()),
        }
    }
}

#[derive(Clone, Debug)]
struct ColorPicker {
    palette: [RGBColor; 2],
    expansion: f64,
    offset: f64,
    scale: f64,
}

impl ColorPicker {
    fn get_color(&self, palette_index: usize, count: u32) -> RGBAColor {
        let intensity = self.scale * ((self.expansion * count as f64).log2() - self.offset);
        self.palette[palette_index].mix(intensity.clamp(0.0, 1.0))
    }
}

// wrapper of Block to make drawable on Plotter's DrawingArea
#[derive(Debug)]
struct DrawableBlock<'a> {
    block: &'a Block,
    picker: &'a ColorPicker,
}

impl<'a> DrawableBlock<'a> {
    fn new(block: &'a Block, picker: &'a ColorPicker) -> DrawableBlock<'a> {
        DrawableBlock { block, picker }
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a DrawableBlock<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0)) // always anchored at the top left corner
    }
}

impl<DB> Drawable<DB> for DrawableBlock<'_>
where
    DB: DrawingBackend,
{
    fn draw<I>(&self, pos: I, backend: &mut DB, _: (u32, u32)) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        I: Iterator<Item = (i32, i32)>,
    {
        let mut pos = pos;
        let pos = pos.next().unwrap();
        for (y, line) in self.block.cnt.chunks(self.block.width).rev().enumerate() {
            for (x, cnt) in line.iter().enumerate() {
                let cf = self.picker.get_color(0, cnt[0]).color();
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cf)?;

                let cr = self.picker.get_color(1, cnt[1]).color();
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cr)?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TickPitch {
    base_per_pixel: u32,
    label_period: u32,
    pitch_in_bases: u32,
    pixels_per_pitch: f64,
    subunit: u32,
}

impl TickPitch {
    pub fn new(desired_pitch_in_pixels: u32, base_per_pixel: u32) -> TickPitch {
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
        TickPitch {
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
pub struct TickAppearance<'a> {
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
        range: &Range<usize>,
        axis_direction: Direction,
        tick_direction: Direction,
        pitch: &'_ TickPitch,
        app: &'a TickAppearance,
        label_formatter: F,
    ) -> Vec<Tick<'a>>
    where
        F: Fn(u32, u32) -> String,
    {
        let range = range.start as u32..range.end as u32;
        let base_to_pixel = pitch.pixels_per_pitch / pitch.pitch_in_bases as f64;
        let build = |i: u32, is_large: bool, show_label: bool| {
            Self::build(
                root,
                ((i - range.start) as f64 * base_to_pixel) as i32,
                tick_direction,
                axis_direction,
                (i == range.start, i == range.end),
                is_large,
                app,
                label_formatter(i, pitch.subunit),
                show_label,
            )
        };

        let mut labels = Vec::new();
        labels.push(build(range.start, true, true));

        let mut i = range.start;
        loop {
            let next_n = (i + 1).div_ceil(pitch.pitch_in_bases);
            i = next_n * pitch.pitch_in_bases;
            if i >= range.end {
                break;
            }

            let thresh = pitch.pitch_in_bases * 3;
            let too_close_to_end = i <= range.start + thresh || i + thresh >= range.end;
            let is_large = next_n % pitch.label_period == 0;
            let show_label = is_large && !too_close_to_end;
            labels.push(build(i, is_large, show_label));
        }
        labels.push(build(range.end, true, true));
        labels
    }

    fn build(
        root: (i32, i32),
        pos: i32,
        tick_direction: Direction,
        axis_direction: Direction,
        is_end: (bool, bool),
        is_large: bool,
        app: &'a TickAppearance,
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
    thickness: u32,
    pitch: TickPitch,
    app: &'a TickAppearance<'a>,
}

impl<'a> LengthScale<'a> {
    fn new(desired_length: u32, axis_thickness: u32, tick_pitch: &'_ TickPitch, tick_appearance: &'a TickAppearance) -> LengthScale<'a> {
        let len = (tick_pitch.label_period * tick_pitch.pitch_in_bases).div_ceil(desired_length) * desired_length;
        LengthScale {
            len,
            thickness: axis_thickness,
            pitch: tick_pitch.clone(),
            app: tick_appearance,
        }
    }

    fn get_dim(&self) -> (u32, u32) {
        let w = self.len + 1;
        let h = self.app.large_tick_length + self.thickness + self.app.label_setback + self.app.label_style.font.get_size() as u32;
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
            &(0..self.len as usize),
            Direction::Right,
            Direction::Down,
            &self.pitch,
            &self.app,
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
                height: self.thickness,
            },
            LayoutElem::Rect {
                id: Some("down_ticks".to_string()),
                width,
                height: self.app.large_tick_length,
            },
        ]));

        // extend the first and last ticks upward
        let mut ticks = ticks;
        let adj = self.app.large_tick_length as i32 + self.thickness as i32;
        if let Some(tick) = ticks.first_mut() {
            tick.tick_start.1 -= adj;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label = format!("{} {}bp", &tick.label, self.pitch.get_subunit_text());
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

#[derive(Clone)]
pub struct ColorScale<'a> {
    len: u32,
    thickness: u32,
    color_map: ColorMap,
    pitch: TickPitch,
    app: &'a TickAppearance<'a>,
}

impl<'a> ColorScale<'a> {
    fn new(desired_length: u32, axis_thickness: u32, color_map: &'_ ColorMap, appearance: &'a TickAppearance) -> ColorScale<'a> {
        let pitch = TickPitch::new(desired_length / 4, 1);
        let len = pitch.label_period * pitch.pitch_in_bases;
        ColorScale {
            len,
            thickness: axis_thickness,
            color_map: color_map.clone(),
            pitch,
            app: appearance,
        }
    }

    fn get_dim(&self) -> (u32, u32) {
        let w = self.len + 1;
        let h = self.app.large_tick_length + self.thickness + self.app.label_setback + self.app.label_style.font.get_size() as u32;
        (w, h)
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
            &(0..self.len as usize),
            Direction::Right,
            Direction::Down,
            &self.pitch,
            &self.app,
            |i, _| format!("{:.1}", self.color_map.max_density.powf(i as f64 / self.len as f64)),
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
                height: self.thickness,
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
        let picker = self.color_map.to_picker(self.pitch.base_per_pixel as f64);
        for i in 0..len {
            let cnt = self.color_map.max_density.powf(i as f64 / len as f64);
            let cf = picker.get_color(0, cnt as u32).color();
            backend.draw_rect(fw_shift((i, 0)), fw_shift((i + 1, height)), &cf, true)?;

            let cr = picker.get_color(1, cnt as u32).color();
            backend.draw_rect(rv_shift((i, 0)), rv_shift((i + 1, height)), &cr, true)?;
        }

        // draw ticks
        let mut ticks = ticks;
        let adj = self.app.large_tick_length as i32 + self.thickness as i32;
        if let Some(tick) = ticks.first_mut() {
            tick.tick_start.1 -= adj;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label = format!("{}/kbp^2", &tick.label);
            tick.tick_start.1 -= adj;
        }

        let range = layout.get_range("rv").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft));
        for (i, tick) in ticks.iter().enumerate() {
            let tick = Tick {
                show_label: i % 2 == 0,
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

#[derive(Clone)]
pub struct DotPlotAppearance<'a> {
    pub axis_thickness: u32,
    pub spacer_thickness: u32,
    pub desired_tick_pitch: u32,

    pub x_label_area_size: u32,
    pub x_tick_apparance: TickAppearance<'a>,
    pub x_seq_name_style: TextStyle<'a>,
    pub x_seq_name_setback: u32,

    pub y_label_area_size: u32,
    pub y_tick_appearance: TickAppearance<'a>,
    pub y_seq_name_style: TextStyle<'a>,
    pub y_seq_name_setback: u32,
}

#[derive(Clone)]
pub struct DotPlot<'a> {
    tile: &'a BlockTile,
    dim: (u32, u32),
    x_brks: Breakpoints,
    y_brks: Breakpoints,
    pitch: TickPitch,
    picker: ColorPicker,
    app: &'a DotPlotAppearance<'a>,
}

impl<'a> DotPlot<'a> {
    fn new(tile: &'a BlockTile, appearance: &'a DotPlotAppearance<'a>, color_map: &'_ ColorMap) -> DotPlot<'a> {
        let add_spacer = |x: &u32| *x + appearance.spacer_thickness;
        let x_pixels = tile.horizontal_pixels().iter().map(add_spacer).collect::<Vec<_>>();
        let y_pixels = tile.vertical_pixels().iter().map(add_spacer).collect::<Vec<_>>();
        let width = x_pixels.iter().sum::<u32>() + appearance.y_label_area_size;
        let height = y_pixels.iter().sum::<u32>() + appearance.x_label_area_size;
        DotPlot {
            tile,
            dim: (width, height),
            x_brks: Breakpoints::from_pixels(&x_pixels),
            y_brks: Breakpoints::from_pixels(&y_pixels),
            pitch: TickPitch::new(appearance.desired_tick_pitch, tile.base_per_pixel() as u32),
            picker: ColorMap::to_picker(color_map, tile.base_per_pixel() as f64),
            app: appearance,
        }
    }

    fn get_dim(&self) -> (u32, u32) {
        self.dim
    }

    fn draw_block<DB>(&self, pos: (i32, i32), backend: &mut DB, block: &Block) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);

        let layout = Layout(LayoutElem::Margined {
            margin: LayoutMargin::new(0, self.app.spacer_thickness, self.app.spacer_thickness, 0),
            center: Box::new(LayoutElem::Rect {
                id: None,
                width: block.width as u32,
                height: block.height as u32,
            }),
        });

        let spacer_color = RGBColor(192, 208, 192).color();
        for area in &[".top", ".top-right", ".right"] {
            let range = layout.get_range(area).unwrap();
            backend.draw_rect(
                shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft)),
                shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomRight)),
                &spacer_color,
                true,
            )?;
        }
        let range = layout.get_range(".center").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft));
        DrawableBlock::new(block, &self.picker).draw(std::iter::once(pos), backend, (0, 0))?;
        Ok(())
    }

    fn draw_tile<DB>(&self, pos: (i32, i32), backend: &mut DB) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);

        // eprintln!("draw_tile: {}, {}", base_x, base_y);
        for (i, y) in self.y_brks.as_anchor_slice().windows(2).enumerate() {
            let blocks = self.tile.get_row(i).unwrap();
            for (x, block) in self.x_brks.as_anchor_slice().windows(2).zip(blocks.iter()) {
                // eprintln!("draw_block: {}, {}, {}", i, base_x + x[0] as i32, base_y - y[1] as i32);
                self.draw_block(shift((x[0] as i32, -(y[1] as i32))), backend, block)?;
            }
        }
        Ok(())
    }

    fn draw_xlabel<DB, FM, FF>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        seq: &Seq,
        range_mapper: FM,
        label_formatter: FF,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        FM: Fn(&Seq) -> Range<usize>,
        FF: Fn(u32, u32) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        let ticks = Tick::build_vec(
            (0, 0),
            &range_mapper(seq),
            Direction::Right,
            Direction::Down,
            &self.pitch,
            &self.app.x_tick_apparance,
            label_formatter,
        );
        let width = ticks.last().unwrap().tick_start.0.abs() as u32 + 1;
        let layout = Layout(LayoutElem::Vertical(vec![
            LayoutElem::Rect {
                id: Some("axis".to_string()),
                width,
                height: self.app.axis_thickness,
            },
            LayoutElem::Rect {
                id: Some("ticks".to_string()),
                width,
                height: self.app.x_seq_name_setback,
            },
            LayoutElem::Rect {
                id: Some("seq_names".to_string()),
                width,
                height: 0,
            },
        ]));
        let range = layout.get_range("axis").unwrap();
        backend.draw_rect(
            shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft)),
            shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomRight)),
            &BLACK.color(),
            true,
        )?;

        let range = layout.get_range("ticks").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft));
        for tick in ticks {
            tick.draw(std::iter::once(pos), backend, (0, 0))?;
        }

        let style = &self.app.x_seq_name_style.pos(Pos::new(HPos::Center, VPos::Top));
        let range = layout.get_range("seq_names").unwrap();
        let (x, y) = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft);
        backend.draw_text(&seq.name, style, shift((x + width as i32 / 2, y)))?;
        Ok(())
    }

    fn draw_xlabels<DB, FM, FF>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        range_mapper: FM,
        label_formatter: FF,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        FM: Fn(&Seq) -> Range<usize>,
        FF: Fn(u32, u32) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        for (x, seq) in self.x_brks.as_anchor_slice().windows(2).zip(self.tile.horizontal_seqs().iter()) {
            eprintln!("draw_xlabel: {}, {}", x[0], x[1]);
            self.draw_xlabel(shift((x[0] as i32, 0)), backend, seq, &range_mapper, &label_formatter)?;
        }
        Ok(())
    }

    fn draw_ylabel<DB, FM, FF>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        seq: &Seq,
        range_mapper: FM,
        label_formatter: FF,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        FM: Fn(&Seq) -> Range<usize>,
        FF: Fn(u32, u32) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        let ticks = Tick::build_vec(
            (0, 0),
            &range_mapper(seq),
            Direction::Up,
            Direction::Left,
            &self.pitch,
            &self.app.y_tick_appearance,
            label_formatter,
        );
        let height = ticks.last().unwrap().tick_start.1.abs() as u32;
        eprintln!(
            "draw_ylabel: {}, {:?}",
            height,
            ticks.iter().map(|t| t.tick_start).collect::<Vec<_>>()
        );
        let layout = Layout(LayoutElem::Horizontal(vec![
            LayoutElem::Rect {
                id: Some("seq_names".to_string()),
                width: 0,
                height,
            },
            LayoutElem::Rect {
                id: Some("ticks".to_string()),
                width: self.app.y_seq_name_setback,
                height,
            },
            LayoutElem::Rect {
                id: Some("axis".to_string()),
                width: self.app.axis_thickness,
                height,
            },
        ]));

        let range = layout.get_range("axis").unwrap();
        backend.draw_rect(
            shift(range.get_relative_pos(RectAnchor::BottomRight, RectAnchor::TopLeft)),
            shift(range.get_relative_pos(RectAnchor::BottomRight, RectAnchor::BottomRight)),
            &BLACK.color(),
            true,
        )?;

        let range = layout.get_range("ticks").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::BottomRight, RectAnchor::BottomRight));
        for tick in ticks {
            tick.draw(std::iter::once(pos), backend, (0, 0))?;
        }

        let style = &self
            .app
            .y_seq_name_style
            .pos(Pos::new(HPos::Center, VPos::Bottom))
            .transform(FontTransform::Rotate270);
        let range = layout.get_range("seq_names").unwrap();
        let (x, y) = range.get_relative_pos(RectAnchor::BottomRight, RectAnchor::BottomRight);
        backend.draw_text(&seq.name, style, shift((x, y - height as i32 / 2)))?;
        Ok(())
    }

    fn draw_ylabels<DB, FM, FF>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        range_mapper: FM,
        label_formatter: FF,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        FM: Fn(&Seq) -> Range<usize>,
        FF: Fn(u32, u32) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        for (y, seq) in self.y_brks.as_anchor_slice().windows(2).zip(self.tile.vertical_seqs().iter()) {
            self.draw_ylabel(shift((0, -(y[0] as i32))), backend, seq, &range_mapper, &label_formatter)?;
        }
        Ok(())
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a DotPlot<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
    }
}

impl<DB> Drawable<DB> for DotPlot<'_>
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

        let layout = Layout(LayoutElem::Margined {
            margin: LayoutMargin::new(self.app.y_label_area_size, 0, 0, self.app.x_label_area_size),
            center: Box::new(LayoutElem::Rect {
                id: None,
                width: self.x_brks.pixels(),
                height: self.y_brks.pixels(),
            }),
        });

        let range = layout.get_range(".center").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomLeft);
        self.draw_tile(shift(pos), backend)?;

        let range = layout.get_range(".left").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomRight);
        self.draw_ylabels(
            shift(pos),
            backend,
            |s| s.range.clone(),
            |i, u| format!("{:.1}", i as f64 / u as f64),
        )?;

        let range = layout.get_range(".bottom").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft);
        self.draw_xlabels(
            shift(pos),
            backend,
            |s| s.range.clone(),
            |i, u| format!("{:.1}", i as f64 / u as f64),
        )?;
        Ok(())
    }
}

pub fn plot(name: &str, tile: &BlockTile) -> Result<()> {
    let text_style = TextStyle::from(("sans-serif", 12).into_font()).color(&BLACK);
    let tick_appearance = TickAppearance {
        large_tick_length: 3,
        small_tick_length: 1,
        label_setback: 8,
        label_style: text_style.clone(),
        fit_in_box: true,
    };
    let appearance = DotPlotAppearance {
        axis_thickness: 1,
        spacer_thickness: 1,
        desired_tick_pitch: 25,

        x_label_area_size: 35,
        x_tick_apparance: tick_appearance.clone(),
        x_seq_name_style: text_style.clone(),
        x_seq_name_setback: 20,

        y_label_area_size: 50,
        y_tick_appearance: tick_appearance.clone(),
        y_seq_name_style: text_style.clone(),
        y_seq_name_setback: 35,
    };
    let color_map = ColorMap {
        palette: [RGBColor(255, 0, 64), RGBColor(0, 64, 255)],
        max_density: 1000.0,
        min_density: 1.0,
    };
    let dotplot = DotPlot::new(tile, &appearance, &color_map);

    let (width, height) = dotplot.get_dim();
    let layout = Layout(LayoutElem::Margined {
        margin: LayoutMargin::uniform(20),
        center: Box::new(LayoutElem::Vertical(vec![
            LayoutElem::Horizontal(vec![
                LayoutElem::Rect {
                    id: None,
                    width: 50,
                    height: 30,
                },
                LayoutElem::Rect {
                    id: Some("length_scale".to_string()),
                    width: 110,
                    height: 30,
                },
                LayoutElem::Rect {
                    id: Some("color_scale".to_string()),
                    width: 110,
                    height: 30,
                },
            ]),
            LayoutElem::Rect {
                id: Some("dotplot".to_string()),
                width,
                height,
            },
        ])),
    });
    let areas = StructuredDrawingArea::from_layout(&layout, name)?;

    if let Some(area) = areas.get_area("dotplot") {
        area.draw(&dotplot)?;
    }
    if let Some(area) = areas.get_area("length_scale") {
        let tick_pitch = TickPitch::new(appearance.desired_tick_pitch, tile.base_per_pixel() as u32);
        let tick_appearance = TickAppearance {
            fit_in_box: false,
            ..tick_appearance.clone()
        };
        let length_scale = LengthScale::new(100, 1, &tick_pitch, &tick_appearance);
        area.draw(&length_scale)?;
    }
    if let Some(area) = areas.get_area("color_scale") {
        let tick_appearance = TickAppearance {
            fit_in_box: false,
            ..tick_appearance.clone()
        };
        let color_scale = ColorScale::new(200, 1, &color_map, &tick_appearance);
        area.draw(&color_scale)?;
    }
    areas.present()?;
    Ok(())
}
