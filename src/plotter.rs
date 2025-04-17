// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use crate::block::{Block, BlockTile};
use crate::seq::Seq;
use anyhow::{Result, anyhow};
use plotters::coord::Shift;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug)]
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

    fn as_slice(&self) -> &[u32] {
        assert!(self.v.len() >= 2);
        &self.v[1..self.v.len() - 1]
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

struct LayoutMargin {
    left: u32,
    right: u32,
    top: u32,
    bottom: u32,
}

impl LayoutMargin {
    fn new(left: u32, right: u32, top: u32, bottom: u32) -> LayoutMargin {
        LayoutMargin { left, right, top, bottom }
    }

    fn uniform(margin: u32) -> LayoutMargin {
        LayoutMargin {
            left: margin,
            right: margin,
            top: margin,
            bottom: margin,
        }
    }
}

enum LayoutElem {
    Rect(String, u32, u32),
    Horizontal(String, Vec<LayoutElem>),
    Vertical(String, Vec<LayoutElem>),
    Margined(String, LayoutMargin, Box<LayoutElem>),
}

impl LayoutElem {
    fn get_dim(&self) -> (u32, u32) {
        match self {
            LayoutElem::Rect(_, w, h) => (*w, *h),
            LayoutElem::Horizontal(_, elems) => elems
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0 + w, acc.1.max(h))),
            LayoutElem::Vertical(_, elems) => elems
                .iter()
                .map(|x| x.get_dim())
                .fold((0, 0), |acc, (w, h)| (acc.0.max(w), acc.1 + h)),
            LayoutElem::Margined(_, margin, elem) => {
                let (w, h) = elem.get_dim();
                (w + margin.left + margin.right, h + margin.top + margin.bottom)
            }
        }
    }

    fn get_anchors(&self) -> (Vec<u32>, Vec<u32>) {
        match self {
            LayoutElem::Rect(_, w, h) => (vec![0, *w], vec![0, *h]),
            LayoutElem::Horizontal(_, elems) => {
                let mut v = vec![0];
                let mut w_acc = 0;
                let mut h_max = 0;
                for elem in elems.iter() {
                    let (w, h) = elem.get_dim();
                    w_acc += w;
                    v.push(w_acc);
                    h_max = h_max.max(h);
                }
                (v, vec![0, h_max])
            }
            LayoutElem::Vertical(_, elems) => {
                let mut v = vec![0];
                let mut w_max = 0;
                let mut h_acc = 0;
                for elem in elems.iter() {
                    let (w, h) = elem.get_dim();
                    h_acc += h;
                    v.push(h_acc);
                    w_max = w_max.max(w);
                }
                (vec![0, w_max], v)
            }
            LayoutElem::Margined(_, margin, elem) => {
                let (w, h) = elem.get_dim();
                (
                    vec![0, margin.left, margin.left + w, margin.left + w + margin.right],
                    vec![0, margin.top, margin.top + h, margin.top + h + margin.bottom],
                )
            }
        }
    }

    fn get_breakpoints(&self) -> (Vec<u32>, Vec<u32>) {
        let (w, h) = self.get_anchors();
        assert!(w.len() >= 2 && h.len() >= 2);

        (w[1..w.len() - 1].to_vec(), h[1..h.len() - 1].to_vec())
    }
}

struct StructuredDrawingArea<'a> {
    areas: Vec<DrawingArea<BitMapBackend<'a>, Shift>>,
    index: HashMap<String, usize>,
}

impl<'a> StructuredDrawingArea<'a> {
    const MARGIN_TAGS: [&'static str; 9] = [
        "left-top",
        "top",
        "right-top",
        "left",
        "center",
        "right",
        "left-bottom",
        "bottom",
        "right-bottom",
    ];

    fn append_key(&mut self, key: &str) -> Result<()> {
        if self.index.contains_key(key) {
            return Err(anyhow!("duplicate name: {key}"));
        }
        self.index.insert(key.to_string(), self.areas.len() - 1);
        Ok(())
    }

    fn append_elem(&mut self, parent_key: &str, parent_area: &DrawingArea<BitMapBackend<'a>, Shift>, elem: &LayoutElem) -> Result<()> {
        let (wbrk, hbrk) = elem.get_breakpoints();
        let areas = parent_area.split_by_breakpoints(&wbrk, &hbrk);
        match elem {
            LayoutElem::Rect(name, _, _) => {
                self.areas.push(areas[0].clone());

                let key = format!("{parent_key}.{name}");
                self.append_key(&key)?;
            }
            LayoutElem::Horizontal(name, elems) => {
                assert!(hbrk.is_empty());
                for (i, (elem, area)) in elems.iter().zip(areas.iter()).enumerate() {
                    self.areas.push(area.clone());

                    let key = format!("{parent_key}.{name}[{i}]");
                    self.append_key(&key)?;
                    self.append_elem(&key, area, elem)?;
                }
            }
            LayoutElem::Vertical(name, elems) => {
                assert!(wbrk.is_empty());
                for (i, (elem, area)) in elems.iter().zip(areas.iter()).enumerate() {
                    self.areas.push(area.clone());

                    let key = format!("{parent_key}.{name}[{i}]");
                    self.append_key(&key)?;
                    self.append_elem(&key, area, elem)?;
                }
            }
            LayoutElem::Margined(name, _, elem) => {
                assert!(wbrk.len() == 2 && hbrk.len() == 2);
                for (i, area) in areas.iter().enumerate() {
                    self.areas.push(area.clone());

                    let tag = Self::MARGIN_TAGS[i];
                    let key = format!("{parent_key}.{name}[{tag}]");
                    self.append_key(&key)?;
                    if i == 4 {
                        self.append_elem(&key, area, elem)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn from_layout(root_elem: &LayoutElem, name: &'a str) -> Result<StructuredDrawingArea<'a>> {
        let mut s = StructuredDrawingArea {
            areas: Vec::new(),
            index: HashMap::new(),
        };

        let root_area = BitMapBackend::new(name, root_elem.get_dim()).into_drawing_area();
        root_area.fill(&WHITE)?;

        s.areas.push(root_area.clone());
        s.append_key(".")?;
        s.append_elem("", &root_area, root_elem)?;
        Ok(s)
    }

    fn get_area(&self, key: &str) -> Result<&DrawingArea<BitMapBackend<'a>, Shift>> {
        if let Some(&i) = self.index.get(key) {
            Ok(&self.areas[i])
        } else {
            Err(anyhow!("area not found: {}", key))
        }
    }

    fn present(&self) -> Result<()> {
        self.areas[0].present()?;
        Ok(())
    }
}

#[derive(Debug)]
struct ColorMap {
    palette: Vec<(u8, u8, u8)>,
    density: f64,
}

impl ColorMap {
    fn new(palette: &[(u8, u8, u8)], density: f64) -> ColorMap {
        ColorMap {
            palette: palette.to_vec(),
            density,
        }
    }

    fn get_color(&self, palette_index: &[usize], val: u32, base_per_pixel: f64) -> (u8, u8, u8) {
        let min = self.palette[palette_index[0]];
        let max = self.palette[palette_index[1]];

        let blend = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
            let r = (min.0 as u32 * (256 - val) + max.0 as u32 * val) / 256;
            let g = (min.1 as u32 * (256 - val) + max.1 as u32 * val) / 256;
            let b = (min.2 as u32 * (256 - val) + max.2 as u32 * val) / 256;

            (r as u8, g as u8, b as u8)
        };

        let target_count = self.density * (base_per_pixel / 1000.0).powf(2.0);
        eprintln!("target_count: {}, val: {}", target_count, val);
        let val = (val as f64 / target_count).log2() + 64.0;
        let val = (val as i32).clamp(0, 256) as u32;
        blend(min, max, val)
    }

    fn get_rgb_color(&self, palette_index: &[usize], val: u32, base_per_pixel: f64) -> RGBColor {
        let (r, g, b) = self.get_color(palette_index, val, base_per_pixel);
        RGBColor(r, g, b)
    }
}

// wrapper of Block to make drawable on Plotter's DrawingArea
#[derive(Debug)]
struct DrawableBlock<'a> {
    block: &'a Block,
    color_map: &'a ColorMap,
}

impl<'a> DrawableBlock<'a> {
    fn new(block: &'a Block, color_map: &'a ColorMap) -> DrawableBlock<'a> {
        DrawableBlock { block, color_map }
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
                let c = std::cmp::min(
                    self.color_map.get_color(&[0, 1], cnt[0], self.block.base_per_pixel as f64),
                    self.color_map.get_color(&[0, 2], cnt[1], self.block.base_per_pixel as f64),
                );
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), RGBColor(c.0, c.1, c.2).color())?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
enum TickDirection {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Copy, Clone, Debug)]
struct TickPitch {
    label_period: u32,
    pitch_in_bases: u32,
    pixels_per_pitch: f64,
    subunit: u32,
}

#[derive(Clone)]
struct Tick {
    tick_start: (i32, i32),
    tick_end: (i32, i32),
    label_pos: (i32, i32),
    label_anchor: Pos,
    label_font_size: u32,
    label: String,
    show_label: bool,
}

impl TickPitch {
    fn new(target_pixels: u32, base_per_pixel: u32) -> TickPitch {
        let unit_bases = (target_pixels as f64 * base_per_pixel as f64).log(10.0);
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
            label_period,
            pitch_in_bases: pitch_in_bases as u32,
            pixels_per_pitch: pitch_in_bases as f64 / base_per_pixel as f64,
            subunit,
        }
    }

    fn to_ticks<F>(&self, root: (i32, i32), range: &Range<usize>, config: &TickConfig, label_formatter: F) -> Vec<Tick>
    where
        F: Fn(u32, u32) -> String,
    {
        let range = range.start as u32..range.end as u32;
        let base_to_pixel = self.pixels_per_pitch / self.pitch_in_bases as f64;
        let to_pixel = |i: u32, extra: i32| ((i - range.start) as f64 * base_to_pixel) as i32 + extra;
        let fmt = |i: u32| label_formatter(i, self.subunit);

        let mut labels = Vec::new();
        labels.push(config.build_tick(root, to_pixel(range.start, -1), (true, false), true, fmt(range.start), true));

        let mut i = range.start;
        loop {
            let next_n = (i + 1).div_ceil(self.pitch_in_bases);
            i = next_n * self.pitch_in_bases;
            if i >= range.end {
                break;
            }

            let thresh = self.pitch_in_bases * 2;
            let too_close_to_end = i <= range.start + thresh || i + thresh >= range.end;
            let is_large = next_n % self.label_period == 0;
            let show_label = is_large && !too_close_to_end;
            labels.push(config.build_tick(root, to_pixel(i, 0), (false, false), is_large, fmt(i), show_label));
        }

        labels.push(config.build_tick(root, to_pixel(range.end, 1), (false, true), true, fmt(range.end), true));
        labels
    }
}

#[derive(Copy, Clone, Debug)]
struct TickConfig {
    len: (u32, u32),
    label_setback: u32,
    label_font_size: u32,
    tick_direction: TickDirection,
    axis_direction: TickDirection,
}

impl TickConfig {
    fn build_tick(&self, root: (i32, i32), pos: i32, is_end: (bool, bool), is_large: bool, label: String, show_label: bool) -> Tick {
        let pos = match self.axis_direction {
            TickDirection::Up => (root.0, root.1 - pos),
            TickDirection::Down => (root.0, root.1 + pos),
            TickDirection::Left => (root.0 - pos, root.1),
            TickDirection::Right => (root.0 + pos, root.1),
        };
        let len = if is_large { self.len.0 } else { self.len.1 } as i32;
        let (tick_start, tick_end) = match self.tick_direction {
            TickDirection::Up => ((pos.0, pos.1 - len), (pos.0, pos.1)),
            TickDirection::Down => ((pos.0, pos.1), (pos.0, pos.1 + len)),
            TickDirection::Left => ((pos.0 - len, pos.1), (pos.0, pos.1)),
            TickDirection::Right => ((pos.0, pos.1), (pos.0 + len, pos.1)),
        };

        let setback = self.label_setback as i32;
        let end_index = match (is_end.1, is_end.0) {
            (false, true) | (true, true) => 0,
            (false, false) => 1,
            (true, false) => 2,
        };
        let x_anchors = [HPos::Left, HPos::Center, HPos::Right];
        let y_anchors = [VPos::Bottom, VPos::Center, VPos::Top];
        let shift = (self.label_font_size as f64 / 8.0) as i32;
        let shifts = [shift, 0, -shift];
        let (label_pos, label_anchor) = match self.tick_direction {
            TickDirection::Up => (
                (pos.0 + shifts[end_index], pos.1 - setback),
                Pos::new(x_anchors[end_index], VPos::Bottom),
            ),
            TickDirection::Down => (
                (pos.0 + shifts[end_index], pos.1 + setback),
                Pos::new(x_anchors[end_index], VPos::Top),
            ),
            TickDirection::Left => ((pos.0 - setback, pos.1), Pos::new(HPos::Right, y_anchors[end_index])),
            TickDirection::Right => ((pos.0 + setback, pos.1), Pos::new(HPos::Left, y_anchors[end_index])),
        };
        Tick {
            tick_start,
            tick_end,
            label_pos,
            label_anchor,
            label_font_size: self.label_font_size,
            label,
            show_label,
        }
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a Tick {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
    }
}

impl<DB> Drawable<DB> for Tick
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
            let style = &TextStyle::from(("sans-serif", self.label_font_size as u32).into_font())
                .color(&BLACK)
                .pos(self.label_anchor);
            let pos = (pos.0 + self.label_pos.0, pos.1 + self.label_pos.1);
            backend.draw_text(&self.label, style, pos)?;
        }
        Ok(())
    }
}

pub struct PlotterConfig {
    margin: u32,
    spacer_thickness: u32,
    x_label_area_height: u32,
    y_label_area_width: u32,
    legend_left_margin: u32,
    legend_bottom_margin: u32,
    color_scale_height: u32,
    color_scale_width: u32,
    color_scale_length: u32,
    length_scale_height: u32,
    length_scale_width: u32,
    axes_thickness: u32,
    target_tick_pitch: u32,
    x_tick: TickConfig,
    y_tick: TickConfig,
    x_seq_name_setback: u32,
    y_seq_name_setback: u32,
    seq_name_font_size: u32,
}

pub struct Plotter<'a> {
    color_map: ColorMap,
    c: PlotterConfig,
    x_seq_name_style: TextStyle<'a>,
    y_seq_name_style: TextStyle<'a>,
}

impl<'a> Plotter<'a> {
    pub fn new(density: f64, swap_plot_axes: bool) -> Plotter<'a> {
        assert!(!swap_plot_axes);

        let c = PlotterConfig {
            margin: 20,
            spacer_thickness: 1,
            x_label_area_height: 40,
            y_label_area_width: 40,
            legend_left_margin: 40,
            legend_bottom_margin: 10,
            color_scale_height: 20,
            color_scale_width: 150,
            color_scale_length: 100,
            length_scale_height: 20,
            length_scale_width: 150,
            axes_thickness: 1,
            target_tick_pitch: 25,
            x_tick: TickConfig {
                len: (3, 1),
                label_setback: 5,
                label_font_size: 12,
                tick_direction: TickDirection::Down,
                axis_direction: TickDirection::Right,
            },
            y_tick: TickConfig {
                len: (3, 1),
                label_setback: 5,
                label_font_size: 12,
                tick_direction: TickDirection::Left,
                axis_direction: TickDirection::Up,
            },
            x_seq_name_setback: 25,
            y_seq_name_setback: 35,
            seq_name_font_size: 12,
        };
        let color_map = ColorMap::new(&[(255, 255, 255), (255, 0, 64), (0, 64, 255)], density);
        let seq_name_style = TextStyle::from(("sans-serif", c.seq_name_font_size).into_font()).color(&BLACK);
        let x_seq_name_style = seq_name_style.pos(Pos::new(HPos::Center, VPos::Top));
        let y_seq_name_style = seq_name_style
            .transform(FontTransform::Rotate270)
            .pos(Pos::new(HPos::Center, VPos::Bottom));
        Plotter {
            color_map,
            c,
            x_seq_name_style,
            y_seq_name_style,
        }
    }

    fn draw_block(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, block: &Block, color_map: &ColorMap) -> Result<()> {
        let areas = area.split_by_breakpoints([block.width as u32], [self.c.spacer_thickness]);
        let spacer_color = RGBColor(192, 208, 192);
        areas[0].fill(&spacer_color)?;
        areas[1].fill(&spacer_color)?;
        areas[2].draw(&DrawableBlock::new(block, color_map))?;
        areas[3].fill(&spacer_color)?;
        Ok(())
    }

    fn draw_tile(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, tile: &BlockTile, brks: &(Breakpoints, Breakpoints)) -> Result<()> {
        let areas = area.split_by_breakpoints(brks.0.as_slice(), brks.1.as_slice());
        for (i, area_chunk) in areas.chunks(brks.0.segments()).rev().enumerate() {
            let blocks = tile.get_row(i);
            for (area, block) in area_chunk.iter().zip(blocks.iter()) {
                self.draw_block(area, block, &self.color_map)?;
            }
        }
        Ok(())
    }

    fn draw_xlabel(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, seq: &[Seq], brks: &Breakpoints, pitch: &TickPitch) -> Result<()> {
        let (w, _) = area.dim_in_pixel();
        area.draw(&Rectangle::new([(-1, 0), (w as i32, self.c.axes_thickness as i32)], BLACK.filled()))?;

        let areas = area.split_by_breakpoints(brks.as_slice(), &[] as &[u32]);
        for (area, seq) in areas.iter().zip(seq.iter()) {
            let (w, _) = area.dim_in_pixel();
            let ticks = pitch.to_ticks((0, 0), &seq.range, &self.c.x_tick, |i, subunit| {
                format!("{:.1}", i as f64 / subunit as f64)
            });
            for tick in &ticks {
                area.draw(tick)?;
            }
            area.draw_text(&seq.name, &self.x_seq_name_style, (w as i32 / 2, self.c.x_seq_name_setback as i32))?;
        }
        Ok(())
    }

    fn draw_ylabel(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, seq: &[Seq], brks: &Breakpoints, pitch: &TickPitch) -> Result<()> {
        let (w, h) = area.dim_in_pixel();
        area.draw(&Rectangle::new(
            [(w as i32 - self.c.axes_thickness as i32, 0), (w as i32, h as i32 + 1)],
            BLACK.filled(),
        ))?;

        let areas = area.split_by_breakpoints(&[] as &[u32], brks.as_slice());
        for (area, seq) in areas.iter().rev().zip(seq.iter()) {
            let (w, h) = area.dim_in_pixel();
            let ticks = pitch.to_ticks((w as i32 - 1, h as i32 - 1), &seq.range, &self.c.y_tick, |i, subunit| {
                format!("{:.1}", i as f64 / subunit as f64)
            });
            for tick in &ticks {
                area.draw(tick)?;
            }
            area.draw_text(
                &seq.name,
                &self.y_seq_name_style,
                (w as i32 - self.c.y_seq_name_setback as i32, h as i32 / 2),
            )?;
        }
        Ok(())
    }

    fn draw_length_scale(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, tile: &BlockTile) -> Result<()> {
        let pitch = TickPitch::new(self.c.target_tick_pitch, tile.base_per_pixel() as u32);
        let subunit = ["bp", "kbp", "Mbp", "Gbp"][pitch.subunit.ilog(10) as usize / 3];

        // ticks
        let mut ticks = pitch.to_ticks(
            (0, self.c.x_tick.len.0 as i32),
            &(0..(pitch.label_period * pitch.pitch_in_bases) as usize),
            &self.c.x_tick,
            |i, subunit| format!("{:.1}", i as f64 / subunit as f64),
        );

        // slightly modify the first and last ticks
        let anchor = Pos::new(HPos::Center, VPos::Top);
        let adj = self.c.x_tick.len.0 - self.c.axes_thickness + 1;
        if let Some(tick) = ticks.first_mut() {
            tick.label_anchor = anchor;
            tick.tick_start.1 -= adj as i32;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label_anchor = anchor;
            tick.label = format!("{} {}", &tick.label, subunit);
            tick.tick_start.1 -= adj as i32;
        }
        for tick in &ticks {
            area.draw(tick)?;
        }

        // draw axis
        let w = ticks.last().unwrap().tick_start.0 + 1;
        area.draw(&Rectangle::new(
            [
                (0, self.c.x_tick.len.0 as i32),
                (w, self.c.x_tick.len.0 as i32 + self.c.axes_thickness as i32),
            ],
            BLACK.filled(),
        ))?;
        Ok(())
    }

    fn draw_color_scale(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, tile: &BlockTile) -> Result<()> {
        let max_seeds = 1000f64;
        let pitch = TickPitch::new(self.c.color_scale_length / 3, 1);
        eprintln!("pitch: {:?}", pitch);

        let areas = area.split_by_breakpoints(
            &[] as &[u32],
            &[
                self.c.x_tick.len.0,
                self.c.x_tick.len.0 + self.c.axes_thickness,
                self.c.x_tick.len.0 + self.c.axes_thickness + self.c.x_tick.len.0,
            ],
        );

        // draw color scale
        let base_per_pixel = tile.base_per_pixel() as f64;
        for i in 0..self.c.color_scale_length {
            let cnt = max_seeds.powf(i as f64 / self.c.color_scale_length as f64);
            let cf = self.color_map.get_rgb_color(&[0, 1], cnt as u32, base_per_pixel).filled();
            let cr = self.color_map.get_rgb_color(&[0, 2], cnt as u32, base_per_pixel).filled();
            areas[0].draw(&Rectangle::new([(i as i32 + 1, 0), (i as i32 + 2, self.c.x_tick.len.0 as i32)], cf))?;
            areas[2].draw(&Rectangle::new([(i as i32 + 1, 0), (i as i32 + 2, self.c.x_tick.len.0 as i32)], cr))?;
        }

        // ticks
        let mut ticks = pitch.to_ticks(
            (0, 0),
            &(0..(pitch.label_period * pitch.pitch_in_bases) as usize),
            &self.c.x_tick,
            |i, _| format!("{:.1}", max_seeds.powf(i as f64 / self.c.color_scale_length as f64)),
        );
        let anchor = Pos::new(HPos::Center, VPos::Top);
        let adj = self.c.x_tick.len.0 - self.c.axes_thickness + 1;
        if let Some(tick) = ticks.first_mut() {
            tick.label_anchor = anchor;
            tick.tick_start.1 -= adj as i32;
        }
        if let Some(tick) = ticks.last_mut() {
            tick.label_anchor = anchor;
            tick.label = format!("{}/px^2", &tick.label);
            tick.tick_start.1 -= adj as i32;
        }
        for (i, tick) in ticks.iter().enumerate() {
            areas[1].draw(&Tick {
                show_label: i % 2 == 0,
                ..tick.clone()
            })?;
        }

        // draw axis
        let w = ticks.last().unwrap().tick_start.0 + 1;
        area.draw(&Rectangle::new(
            [
                (0, self.c.x_tick.len.0 as i32),
                (w, self.c.x_tick.len.0 as i32 + self.c.axes_thickness as i32),
            ],
            BLACK.filled(),
        ))?;
        Ok(())
    }

    pub fn plot(&self, name: &str, tile: &BlockTile) -> Result<()> {
        let pitch = TickPitch::new(self.c.target_tick_pitch, tile.base_per_pixel() as u32);
        let pixels = (
            tile.horizontal_pixels()
                .iter()
                .map(|&x| x + self.c.spacer_thickness)
                .collect::<Vec<_>>(),
            tile.vertical_pixels()
                .iter()
                .rev()
                .map(|&x| x + self.c.spacer_thickness)
                .collect::<Vec<_>>(),
        );
        let brks = (Breakpoints::from_pixels(&pixels.0), Breakpoints::from_pixels(&pixels.1));

        let layout = LayoutElem::Margined(
            "root".to_string(),
            LayoutMargin::uniform(self.c.margin as u32),
            Box::new(LayoutElem::Vertical(
                "stack".to_string(),
                vec![
                    LayoutElem::Margined(
                        "legend".to_string(),
                        LayoutMargin::new(self.c.legend_left_margin as u32, 0, 0, self.c.legend_bottom_margin as u32),
                        Box::new(LayoutElem::Horizontal(
                            "stack".to_string(),
                            vec![
                                LayoutElem::Rect(
                                    "length_scale".to_string(),
                                    self.c.length_scale_width as u32,
                                    self.c.length_scale_height as u32,
                                ),
                                LayoutElem::Rect(
                                    "color_scale".to_string(),
                                    self.c.color_scale_width as u32,
                                    self.c.color_scale_height as u32,
                                ),
                            ],
                        )),
                    ),
                    LayoutElem::Margined(
                        "blocks_with_label".to_string(),
                        LayoutMargin::new(self.c.y_label_area_width as u32, 0, 0, self.c.x_label_area_height as u32),
                        Box::new(LayoutElem::Rect("blocks".to_string(), brks.0.pixels(), brks.1.pixels())),
                    ),
                ],
            )),
        );
        let areas = StructuredDrawingArea::from_layout(&layout, name)?;

        self.draw_tile(areas.get_area(".root[center].stack[1].blocks_with_label[center]")?, tile, &brks)?;
        self.draw_ylabel(
            areas.get_area(".root[center].stack[1].blocks_with_label[left]")?,
            tile.vertical_seqs(),
            &brks.1,
            &pitch,
        )?;
        self.draw_xlabel(
            areas.get_area(".root[center].stack[1].blocks_with_label[bottom]")?,
            tile.horizontal_seqs(),
            &brks.0,
            &pitch,
        )?;
        // self.draw_origin(areas.get_area(".root[center].stack[1].blocks_with_label[left-bottom]")?)?;
        self.draw_length_scale(areas.get_area(".root[center].stack[0].legend[center].stack[0].length_scale")?, tile)?;
        self.draw_color_scale(areas.get_area(".root[center].stack[0].legend[center].stack[1].color_scale")?, tile)?;
        areas.present()?;

        Ok(())
    }
}
