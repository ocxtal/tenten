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

enum LayoutElem {
    Rect(String, u32, u32),
    Horizontal(String, Vec<LayoutElem>),
    Vertical(String, Vec<LayoutElem>),
    Margined(String, Box<LayoutElem>, (u32, u32), (u32, u32)),
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
            LayoutElem::Margined(_, elem, (l, r), (t, b)) => {
                let (w, h) = elem.get_dim();
                (w + *l + *r, h + *t + *b)
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
            LayoutElem::Margined(_, elem, (l, r), (t, b)) => {
                let (w, h) = elem.get_dim();
                (vec![0, *l, *l + w, *l + w + *r], vec![0, *t, *t + h, *t + h + *b])
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
            LayoutElem::Margined(name, elem, _, _) => {
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
    count_per_seed: f64,
    scale: f64,
}

impl ColorMap {
    fn new(palette: &[(u8, u8, u8)], count_per_seed: f64, scale: f64) -> ColorMap {
        ColorMap {
            palette: palette.to_vec(),
            count_per_seed,
            scale,
        }
    }

    fn get_color(&self, palette_index: &[usize], val: u32) -> (u8, u8, u8) {
        let min = self.palette[palette_index[0]];
        let max = self.palette[palette_index[1]];

        let blend = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
            let r = (min.0 as u32 * (256 - val) + max.0 as u32 * val) / 256;
            let g = (min.1 as u32 * (256 - val) + max.1 as u32 * val) / 256;
            let b = (min.2 as u32 * (256 - val) + max.2 as u32 * val) / 256;

            (r as u8, g as u8, b as u8)
        };

        let occ = val as f64 * self.count_per_seed;
        let occ = self.scale * occ.log2();
        let occ = (occ as i32).clamp(0, 256) as u32;
        blend(min, max, occ)
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
                let c = std::cmp::min(self.color_map.get_color(&[0, 1], cnt[0]), self.color_map.get_color(&[0, 2], cnt[1]));
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), RGBColor(c.0, c.1, c.2).color())?;
            }
        }
        Ok(())
    }
}

struct Tick {
    pos: (i32, i32),
    len: i32,
    direction: TickDirection,
}

enum TickDirection {
    Up,
    Down,
    Left,
    Right,
}

impl Tick {
    fn new(pos: (u32, u32), len: u32, direction: TickDirection) -> Tick {
        Tick {
            pos: (pos.0 as i32, pos.1 as i32),
            len: len as i32,
            direction,
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
        let pos = (self.pos.0 + pos.0, self.pos.1 + pos.1);

        let style = ShapeStyle {
            color: BLACK.into(),
            filled: false,
            stroke_width: 1,
        };
        match self.direction {
            TickDirection::Up => backend.draw_line((pos.0, pos.1 - self.len), (pos.0, pos.1), &style),
            TickDirection::Down => backend.draw_line((pos.0, pos.1), (pos.0, pos.1 + self.len), &style),
            TickDirection::Left => backend.draw_line((pos.0 - self.len, pos.1), (pos.0, pos.1), &style),
            TickDirection::Right => backend.draw_line((pos.0, pos.1), (pos.0 + self.len, pos.1), &style),
        }
    }
}

struct TickLabel {
    pos: u32,
    text: String,
    is_end: (bool, bool),
    close_to_next: bool,
}

impl TickLabel {
    fn index(&self) -> usize {
        match (self.is_end.1, self.is_end.0) {
            (false, true) | (true, true) => 0,
            (false, false) => 1,
            (true, false) => 2,
        }
    }
}

fn determine_subunit(tick_pitch: (u32, f64)) -> u64 {
    10u64.pow((tick_pitch.0 as f64).log(10.0).floor() as u32 / 3 * 3)
}

fn build_tick_labels(seq: &Seq, tick_pitch: (u32, f64), include_ends: (bool, bool)) -> Vec<TickLabel> {
    let tick_pitch_in_pixel = tick_pitch.0 as usize;
    let pos_to_pixel = tick_pitch.1 / tick_pitch.0 as f64;
    let subunit = determine_subunit(tick_pitch);

    let mut labels = Vec::new();
    let mut push = |seq_pos: usize, extra: usize, is_end: (bool, bool), close_to_next: bool| {
        let tick_pos = ((seq_pos - seq.range.start) as f64 * pos_to_pixel) as u32;
        labels.push(TickLabel {
            pos: tick_pos + extra as u32,
            text: format!("{:.1}", seq_pos as f64 / subunit as f64),
            is_end,
            close_to_next,
        });
    };

    let mut seq_pos = seq.range.start;
    if include_ends.0 {
        push(seq_pos, 0, (true, false), false);
    }
    loop {
        seq_pos = (seq_pos + 1).div_ceil(tick_pitch_in_pixel) * tick_pitch_in_pixel;
        if seq_pos >= seq.range.end {
            break;
        }
        push(seq_pos, 0, (false, false), seq_pos + tick_pitch_in_pixel / 2 >= seq.range.end);
    }
    if include_ends.1 {
        push(seq.range.end, 1, (false, true), false);
    }
    labels
}

pub struct PlotterConfig {
    margin: usize,
    spacer_thickness: usize,
    x_label_area_height: usize,
    y_label_area_width: usize,
    colorbar_area_height: usize,
    axes_thickness: usize,
    tick_len: usize,
    tick_label_setback: usize,
    tick_label_font_size: usize,
    x_seq_name_setback: usize,
    y_seq_name_setback: usize,
    seq_name_font_size: usize,
    count_per_seed: f64,
    scale: f64,
}

pub struct Plotter<'a> {
    color_map: ColorMap,
    c: PlotterConfig,
    tick_label_style: TextStyle<'a>,
    x_seq_name_style: TextStyle<'a>,
    x_seq_name_anchors: Vec<Pos>,
    y_seq_name_style: TextStyle<'a>,
    y_seq_name_anchors: Vec<Pos>,
}

impl<'a> Plotter<'a> {
    pub fn new(count_per_seed: f64, scale: f64) -> Plotter<'a> {
        let c = PlotterConfig {
            margin: 20,
            spacer_thickness: 1,
            x_label_area_height: 40,
            y_label_area_width: 40,
            colorbar_area_height: 30,
            axes_thickness: 1,
            tick_len: 3,
            tick_label_setback: 5,
            tick_label_font_size: 12,
            x_seq_name_setback: 25,
            y_seq_name_setback: 35,
            seq_name_font_size: 12,
            count_per_seed,
            scale,
        };
        assert!(c.tick_len < c.tick_label_setback);
        assert!(c.tick_label_setback < c.x_seq_name_setback);
        assert!(c.tick_label_setback < c.y_seq_name_setback);
        assert!(c.axes_thickness + c.x_seq_name_setback < c.x_label_area_height);
        assert!(c.axes_thickness + c.y_seq_name_setback < c.y_label_area_width);

        let color_map = ColorMap::new(&[(255, 255, 255), (255, 0, 64), (0, 64, 255)], c.count_per_seed, c.scale);
        let tick_label_style = TextStyle::from(("sans-serif", c.tick_label_font_size as u32).into_font()).color(&BLACK);
        let seq_name_style = TextStyle::from(("sans-serif", c.seq_name_font_size as u32).into_font()).color(&BLACK);
        let x_seq_name_style = seq_name_style.pos(Pos::new(HPos::Center, VPos::Top));
        let y_seq_name_style = seq_name_style
            .transform(FontTransform::Rotate270)
            .pos(Pos::new(HPos::Center, VPos::Bottom));

        let x_seq_name_anchors = [
            Pos::new(HPos::Left, VPos::Top),   // for left labels
            Pos::new(HPos::Center, VPos::Top), // for middle labels
            Pos::new(HPos::Right, VPos::Top),  // for right labels
        ];
        let y_seq_name_anchors = [
            Pos::new(HPos::Right, VPos::Bottom), // for bottom labels
            Pos::new(HPos::Right, VPos::Center), // for middle labels
            Pos::new(HPos::Right, VPos::Top),    // for top labels
        ];

        Plotter {
            color_map,
            c,
            tick_label_style,
            x_seq_name_style,
            x_seq_name_anchors: x_seq_name_anchors.to_vec(),
            y_seq_name_style,
            y_seq_name_anchors: y_seq_name_anchors.to_vec(),
        }
    }

    fn determine_tick_pitch(&self, tile: &BlockTile) -> (u32, f64) {
        let unit_pixels = 15.0 * self.c.tick_label_font_size as f64;
        let unit_bases = (unit_pixels * tile.base_per_pixel() as f64).log(10.0);
        let (f, c) = (unit_bases.fract(), unit_bases.floor());
        assert!(f <= 1.0 && c >= 1.0);

        let pitch_in_bases = if f < 2.5f64.log(10.0) {
            10u64.pow(c as u32)
        } else if f < 5.0f64.log(10.0) {
            10u64.pow(c as u32 + 1) / 4
        } else {
            10u64.pow(c as u32 + 1) / 2
        };

        (pitch_in_bases as u32, pitch_in_bases as f64 / tile.base_per_pixel() as f64)
    }

    fn draw_block(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, block: &Block, color_map: &ColorMap) -> Result<()> {
        let areas = area.split_by_breakpoints([block.width as u32], [self.c.spacer_thickness as u32]);
        let spacer_color = RGBColor(192, 208, 192);
        areas[0].fill(&spacer_color).unwrap();
        areas[1].fill(&spacer_color).unwrap();
        areas[2].draw(&DrawableBlock::new(block, color_map)).unwrap();
        areas[3].fill(&spacer_color).unwrap();
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

    fn draw_xlabel(
        &self,
        area: &DrawingArea<BitMapBackend<'_>, Shift>,
        seq: &[Seq],
        brks: &Breakpoints,
        tick_pitch: (u32, f64),
    ) -> Result<()> {
        let (w0, _) = area.estimate_text_size("0", &self.tick_label_style).unwrap();
        let w0 = w0 as i32 / 2;
        let calc_label_pos = |pos: (u32, u32), index: usize| match index {
            0 => (pos.0 as i32 + w0, pos.1 as i32),
            1 => (pos.0 as i32, pos.1 as i32),
            _ => (pos.0 as i32 - w0, pos.1 as i32),
        };

        let areas = area.split_by_breakpoints(brks.as_slice(), &[] as &[u32]);
        for (area, seq) in areas.iter().zip(seq.iter()) {
            let areas = area.split_by_breakpoints(
                &[] as &[u32],
                [
                    self.c.axes_thickness as u32,
                    self.c.axes_thickness as u32 + self.c.tick_label_setback as u32,
                    self.c.axes_thickness as u32 + self.c.x_seq_name_setback as u32,
                ],
            );
            areas[0].fill(&BLACK).unwrap();

            let ticks = build_tick_labels(seq, tick_pitch, (true, true));
            for tick in &ticks {
                if !tick.is_end.0 {
                    areas[1]
                        .draw(&Tick::new((tick.pos, 0), self.c.tick_len as u32, TickDirection::Down))
                        .unwrap();
                }
                if !tick.close_to_next {
                    areas[2]
                        .draw_text(
                            &tick.text,
                            &self.tick_label_style.pos(self.x_seq_name_anchors[tick.index()]),
                            calc_label_pos((tick.pos, 0), tick.index()),
                        )
                        .unwrap();
                }
            }

            let (w, _) = areas[3].dim_in_pixel();
            areas[3].draw_text(&seq.name, &self.x_seq_name_style, (w as i32 / 2, 0)).unwrap();
        }
        Ok(())
    }

    fn draw_ylabel(
        &self,
        area: &DrawingArea<BitMapBackend<'_>, Shift>,
        seq: &[Seq],
        brks: &Breakpoints,
        tick_pitch: (u32, f64),
    ) -> Result<()> {
        let calc_pos = |area: &DrawingArea<BitMapBackend<'_>, Shift>, pos: (u32, u32)| {
            let (w, h) = area.dim_in_pixel();
            (w - pos.0 - 1, h - pos.1 - 1)
        };

        let (w, _) = area.dim_in_pixel();
        let areas = area.split_by_breakpoints(&[] as &[u32], brks.as_slice());
        for (area, seq) in areas.iter().rev().zip(seq.iter()) {
            let (_, h) = area.dim_in_pixel();
            let areas = {
                let mut areas = area.split_by_breakpoints(
                    [
                        w - self.c.axes_thickness as u32 - self.c.y_seq_name_setback as u32,
                        w - self.c.axes_thickness as u32 - self.c.tick_label_setback as u32,
                        w - self.c.axes_thickness as u32,
                    ],
                    &[] as &[u32],
                );
                areas.reverse();
                areas
            };
            areas[0].fill(&BLACK).unwrap();

            let ticks = build_tick_labels(seq, tick_pitch, (true, true));
            for tick in &ticks {
                let pos = calc_pos(&areas[1], (0, tick.pos));
                if !tick.is_end.0 {
                    areas[1].draw(&Tick::new(pos, self.c.tick_len as u32, TickDirection::Left)).unwrap();
                }
                if !tick.close_to_next {
                    let pos = calc_pos(&areas[2], (0, tick.pos));
                    areas[2]
                        .draw_text(
                            &tick.text,
                            &self.tick_label_style.pos(self.y_seq_name_anchors[tick.index()]),
                            (pos.0 as i32, pos.1 as i32),
                        )
                        .unwrap();
                }
            }
            let pos = calc_pos(&areas[3], (0, h / 2));
            areas[3]
                .draw_text(&seq.name, &self.y_seq_name_style, (pos.0 as i32, pos.1 as i32))
                .unwrap();
        }

        Ok(())
    }

    fn draw_origin(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>) -> Result<()> {
        let (w, _) = area.dim_in_pixel();
        let t = self.c.axes_thickness as u32;
        let areas = area.split_by_breakpoints([w - t], [t]);

        areas[1].fill(&BLACK).unwrap();
        areas[0]
            .draw(&Tick::new(
                (areas[0].dim_in_pixel().0 - 1, 0),
                self.c.tick_len as u32,
                TickDirection::Left,
            ))
            .unwrap();
        areas[3]
            .draw(&Tick::new(
                (areas[3].dim_in_pixel().0 - 1, 0),
                self.c.tick_len as u32,
                TickDirection::Down,
            ))
            .unwrap();
        Ok(())
    }

    fn draw_color_scale(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>) -> Result<()> {
        // let areas = area.split_by_breakpoints(&[] as &[u32], [self.colorbar_area_height as u32]);
        // let (w, h) = areas[0].dim_in_pixel();
        // let color_scale_style = ShapeStyle {
        //     color: RGBColor(255, 0, 0),
        //     filled: true,
        //     stroke_width: 1,
        // };
        // for i in 0..w {
        //     let c = RGBColor(
        //         (i * 255 / w) as u8,
        //         (i * 255 / w) as u8,
        //         (i * 255 / w) as u8,
        //     );
        //     areas[0].draw(&Rectangle::new([(i, 0), (i + 1, h)], color_scale_style.color(c)))?;
        // }
        Ok(())
    }

    pub fn plot(&self, name: &str, tile: &BlockTile) -> Result<()> {
        let tick_pitch = self.determine_tick_pitch(tile);
        log::debug!("tick pitch: {:?}", tick_pitch);

        let pixels = (
            tile.horizontal_pixels()
                .iter()
                .map(|&x| x + self.c.spacer_thickness as u32)
                .collect::<Vec<_>>(),
            tile.vertical_pixels()
                .iter()
                .rev()
                .map(|&x| x + self.c.spacer_thickness as u32)
                .collect::<Vec<_>>(),
        );
        let brks = (Breakpoints::from_pixels(&pixels.0), Breakpoints::from_pixels(&pixels.1));

        let layout = LayoutElem::Margined(
            "margin".to_string(),
            Box::new(LayoutElem::Margined(
                "label".to_string(),
                Box::new(LayoutElem::Rect("blocks".to_string(), brks.0.pixels(), brks.1.pixels())),
                (self.c.y_label_area_width as u32, 0),
                (self.c.colorbar_area_height as u32, self.c.x_label_area_height as u32),
            )),
            (self.c.margin as u32, self.c.margin as u32),
            (self.c.margin as u32, self.c.margin as u32),
        );
        let areas = StructuredDrawingArea::from_layout(&layout, name)?;

        self.draw_tile(areas.get_area(".margin[center].label[center]")?, tile, &brks)?;
        self.draw_ylabel(
            areas.get_area(".margin[center].label[left]")?,
            tile.vertical_seqs(),
            &brks.1,
            tick_pitch,
        )?;
        self.draw_xlabel(
            areas.get_area(".margin[center].label[bottom]")?,
            tile.horizontal_seqs(),
            &brks.0,
            tick_pitch,
        )?;
        self.draw_origin(areas.get_area(".margin[center].label[left-bottom]")?)?;
        areas.present()?;

        Ok(())
    }
}
