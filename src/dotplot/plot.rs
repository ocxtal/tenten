// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use crate::dotplot::Direction;
use crate::dotplot::axis::{Axis, AxisAppearance, Tick};
use crate::dotplot::color::{AnnotationColorMap, DensityColorMap};
use crate::dotplot::layout::{Layout, LayoutElem, LayoutMargin, RectAnchor};
use crate::dotplot::plane::DotPlane;
use crate::dotplot::sequence::SequenceRange;
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub struct DotPlotAppearance<'a> {
    pub spacer_thickness: u32,
    pub desired_tick_pitch: u32,

    pub x_label_area_size: u32,
    pub x_axis_appearance: AxisAppearance<'a>,
    pub x_seq_name_style: TextStyle<'a>,
    pub x_seq_name_setback: u32,

    pub y_label_area_size: u32,
    pub y_axis_appearance: AxisAppearance<'a>,
    pub y_seq_name_style: TextStyle<'a>,
    pub y_seq_name_setback: u32,
}

#[derive(Clone, Debug)]
struct StackedSequenceRange<'a> {
    pos_seq_pairs: Vec<(u32, &'a SequenceRange)>,
    pixels: u32,
}

impl<'a> StackedSequenceRange<'a> {
    fn new(seqs: &'a [SequenceRange], base_per_pixel: usize, spacer_thickness: u32) -> StackedSequenceRange<'a> {
        let mut pixels = 0;
        let mut pos_seq_pairs = Vec::new();
        for seq in seqs {
            pos_seq_pairs.push((pixels, seq));
            pixels += seq.range.len().div_ceil(base_per_pixel) as u32 + spacer_thickness;
        }
        StackedSequenceRange { pos_seq_pairs, pixels }
    }
}

#[derive(Debug)]
struct SortedDotPlanes<'a> {
    x_seqs: StackedSequenceRange<'a>,
    y_seqs: StackedSequenceRange<'a>,
    sorted_planes: Vec<&'a DotPlane>,
}

impl<'a> SortedDotPlanes<'a> {
    fn new(dotplot: &'a DotPlot, spacer_thickness: u32) -> SortedDotPlanes<'a> {
        let x_seqs = StackedSequenceRange::new(&dotplot.rseq, dotplot.base_per_pixel, spacer_thickness);
        let y_seqs = StackedSequenceRange::new(&dotplot.qseq, dotplot.base_per_pixel, spacer_thickness);

        let mut planes = dotplot.planes.iter().collect::<Vec<_>>();
        planes.sort_by(|a, b| a.pair_id.cmp(&b.pair_id));
        SortedDotPlanes {
            x_seqs,
            y_seqs,
            sorted_planes: planes,
        }
    }

    fn get_dim(&self) -> (u32, u32) {
        (self.x_seqs.pixels, self.y_seqs.pixels)
    }

    fn get_row(&'a self, row: usize) -> Option<&'a [&'a DotPlane]> {
        let start = row * self.x_seqs.pos_seq_pairs.len();
        let end = start + self.x_seqs.pos_seq_pairs.len();
        if start > end || end > self.sorted_planes.len() {
            return None;
        }
        Some(&self.sorted_planes[start..end])
    }
}

pub struct DotPlot<'a> {
    rseq: Vec<SequenceRange>,
    qseq: Vec<SequenceRange>,
    rdedup: HashSet<String>,
    qdedup: HashSet<String>,
    rmap: HashMap<String, Vec<usize>>,
    qmap: HashMap<String, Vec<usize>>,
    planes: Vec<DotPlane>,
    pair_to_plane: HashMap<usize, usize>,
    base_per_pixel: usize,
    color_map: DensityColorMap,
    app: &'a DotPlotAppearance<'a>,
    tot_size: usize,
}

impl<'a> DotPlot<'a> {
    pub fn new(
        rseq: &[SequenceRange],
        qseq: &[SequenceRange],
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        appearance: &'a DotPlotAppearance<'a>,
    ) -> DotPlot<'a> {
        log::debug!("DotPlot created");
        let mut bin = DotPlot {
            rseq: Vec::new(),
            qseq: Vec::new(),
            rmap: HashMap::new(),
            qmap: HashMap::new(),
            rdedup: HashSet::new(),
            qdedup: HashSet::new(),
            planes: Vec::new(),
            pair_to_plane: HashMap::new(),
            base_per_pixel,
            color_map: *color_map,
            app: appearance,
            tot_size: 0,
        };
        for r in rseq {
            bin.add_target(r);
        }
        for q in qseq {
            bin.add_query(q);
        }
        bin
    }

    pub fn split(mut self) -> Vec<DotPlot<'a>> {
        let mut v = Vec::new();
        for (rid, rseq) in self.rseq.iter().enumerate() {
            for (qid, qseq) in self.qseq.iter().enumerate() {
                let pair_id = (qid << 32) | rid;
                let plane_index = *self.pair_to_plane.get(&pair_id).unwrap();
                let mut plane = std::mem::take(&mut self.planes[plane_index]);
                plane.pair_id = 0;

                v.push(DotPlot {
                    rseq: vec![rseq.clone()],
                    qseq: vec![qseq.clone()],
                    rdedup: HashSet::from([rseq.to_string()]),
                    qdedup: HashSet::from([qseq.to_string()]),
                    rmap: [(rseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    qmap: [(qseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    planes: vec![plane],
                    pair_to_plane: [(0, 0)].into_iter().collect::<HashMap<_, _>>(),
                    base_per_pixel: self.base_per_pixel,
                    color_map: self.color_map,
                    app: self.app,
                    tot_size: 0,
                })
            }
        }
        v
    }

    pub fn has_plane(&self) -> bool {
        !self.rseq.is_empty() && !self.qseq.is_empty()
    }

    pub fn get_dim(&self) -> (u32, u32) {
        let (width, height) = SortedDotPlanes::new(self, self.app.spacer_thickness).get_dim();
        let width = width + self.app.y_label_area_size;
        let height = height + self.app.x_label_area_size;
        (width, height)
    }

    pub fn get_seed_count(&self) -> usize {
        self.planes.iter().map(|x| x.get_seed_count()).sum::<usize>()
    }

    pub fn base_per_pixel(&self) -> usize {
        self.base_per_pixel
    }

    pub fn color_map(&self) -> &DensityColorMap {
        &self.color_map
    }

    pub fn appearance(&'a self) -> &'a DotPlotAppearance<'a> {
        self.app
    }

    pub fn add_target(&mut self, r: &SequenceRange) {
        if !self.rdedup.insert(r.to_string()) {
            return;
        }

        let rid = self.rseq.len();
        if let Some(rmap) = self.rmap.get_mut(&r.name) {
            rmap.push(rid);
        } else {
            self.rmap.insert(r.name.to_string(), vec![rid]);
        }
        self.rseq.push(r.clone());

        for (qid, q) in self.qseq.iter().enumerate() {
            let pair_id = (qid << 32) | rid;
            self.pair_to_plane.insert(pair_id, self.planes.len());

            let plane = DotPlane::with_pair_id(r, q, self.base_per_pixel, &self.color_map, pair_id);
            self.tot_size += plane.cnt.len();
            self.planes.push(plane);
        }
        log::debug!("target added: {:?}, {:}", &r.name, self.tot_size);
    }

    pub fn add_query(&mut self, q: &SequenceRange) {
        if !self.qdedup.insert(q.to_string()) {
            return;
        }

        let qid = self.qseq.len();
        if let Some(qmap) = self.qmap.get_mut(&q.name) {
            qmap.push(qid);
        } else {
            self.qmap.insert(q.name.to_string(), vec![qid]);
        }
        self.qseq.push(q.clone());

        for (rid, r) in self.rseq.iter().enumerate() {
            let pair_id = (qid << 32) | rid;
            self.pair_to_plane.insert(pair_id, self.planes.len());

            let plane = DotPlane::with_pair_id(r, q, self.base_per_pixel, &self.color_map, pair_id);
            self.tot_size += plane.cnt.len();
            self.planes.push(plane);
        }
        log::debug!("query added: {:?}, {:}", &q.name, self.tot_size);
    }

    pub fn add_annotation(&mut self, r: &[SequenceRange], q: &[SequenceRange], color_map: &AnnotationColorMap) {
        let intersection = |x: &SequenceRange, y: &SequenceRange| -> Option<SequenceRange> {
            if x.virtual_name() != y.virtual_name() {
                return None;
            }
            let (xrange, yrange) = (x.virtual_range(), y.virtual_range());
            let start = std::cmp::max(yrange.start - xrange.start, 0);
            let end = std::cmp::min(yrange.end - xrange.start, xrange.len() as isize);
            if start >= end {
                return None;
            }
            Some(SequenceRange {
                name: x.name.clone(),
                range: start as usize..end as usize,
                annotation: None,
                virtual_name: None,
                virtual_start: None,
            })
        };

        for (rid, rseq) in self.rseq.iter().enumerate() {
            let rannot = r.iter().filter_map(|x| intersection(rseq, x)).collect::<Vec<_>>();
            for (qid, qseq) in self.qseq.iter().enumerate() {
                let qannot = q.iter().filter_map(|x| intersection(qseq, x)).collect::<Vec<_>>();
                let pair_id = (qid << 32) | rid;
                if let Some(&plane_index) = self.pair_to_plane.get(&pair_id) {
                    self.planes[plane_index].add_annotation(&rannot, &qannot, color_map);
                }
            }
        }
    }

    pub fn append_seed(&mut self, rname: &str, rpos: usize, is_rev: bool, qname: &str, qpos: usize) {
        if let (Some(rids), Some(qids)) = (self.rmap.get(rname), self.qmap.get(qname)) {
            for &rid in rids {
                let rseq = &self.rseq[rid];
                if !rseq.range.contains(&rpos) {
                    continue;
                }
                for &qid in qids {
                    let qseq = &self.qseq[qid];
                    if !qseq.range.contains(&qpos) {
                        continue;
                    }
                    let pair_id = (qid << 32) | rid;
                    if let Some(&plane_index) = self.pair_to_plane.get(&pair_id) {
                        self.planes[plane_index].append_seed(rpos, qpos, is_rev);
                    }
                }
            }
        }
    }

    pub fn targets(&self) -> &[SequenceRange] {
        &self.rseq
    }

    pub fn queries(&self) -> &[SequenceRange] {
        &self.qseq
    }

    fn draw_block<DB>(&'a self, pos: (i32, i32), backend: &mut DB, plane: &'a DotPlane) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);

        let layout = Layout(LayoutElem::Margined {
            margin: LayoutMargin::new(0, self.app.spacer_thickness, self.app.spacer_thickness, 0),
            center: Box::new(LayoutElem::new_rect(plane.width as u32, plane.height as u32)),
        });

        let spacer_color = RGBColor(192, 208, 192).color();
        for area in &[".top", ".top-right", ".right"] {
            let range = layout.get_range(area).unwrap();
            backend.draw_rect(
                shift(range.get_relative_pos(RectAnchor::BottomLeft, RectAnchor::TopLeft)),
                shift(range.get_relative_pos(RectAnchor::BottomLeft, RectAnchor::BottomRight)),
                &spacer_color,
                true,
            )?;
        }
        let range = layout.get_range(".center").unwrap();
        let pos = shift(range.get_relative_pos(RectAnchor::BottomLeft, RectAnchor::TopLeft));
        plane.draw(std::iter::once(pos), backend, (0, 0))?;
        Ok(())
    }

    fn draw_tile<DB>(
        &'a self,
        pos: (i32, i32),
        backend: &mut DB,
        sorted_planes: &'a SortedDotPlanes,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        for (i, &(y, _)) in sorted_planes.y_seqs.pos_seq_pairs.iter().enumerate() {
            let planes = sorted_planes.get_row(i).unwrap();
            for (&(x, _), plane) in sorted_planes.x_seqs.pos_seq_pairs.iter().zip(planes.iter()) {
                self.draw_block(shift((x as i32, -(y as i32))), backend, plane)?;
            }
        }
        Ok(())
    }

    fn draw_xlabel<DB, F>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        axis: &Axis,
        seq: &SequenceRange,
        label_formatter: F,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        F: Fn(isize, usize) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        let ticks = Tick::build_vec(
            (0, 0),
            seq.virtual_range(),
            Direction::Right,
            Direction::Down,
            axis,
            &self.app.x_axis_appearance,
            label_formatter,
        );
        let width = ticks.last().unwrap().tick_start.0.unsigned_abs() + 1;
        let layout = Layout(LayoutElem::Vertical(vec![
            LayoutElem::new_rect_with_id(width, self.app.x_axis_appearance.axis_thickness, "axis"),
            LayoutElem::new_rect_with_id(width, self.app.x_seq_name_setback, "ticks"),
            LayoutElem::new_rect_with_id(width, 0, "seq_names"),
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
        backend.draw_text(seq.virtual_name(), style, shift((x + width as i32 / 2, y)))?;
        Ok(())
    }

    fn draw_xlabels<DB, F>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        axis: &Axis,
        sorted_planes: &SortedDotPlanes,
        label_formatter: F,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        F: Fn(isize, usize) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        for &(x, seq) in sorted_planes.x_seqs.pos_seq_pairs.iter() {
            self.draw_xlabel(shift((x as i32, 0)), backend, axis, seq, &label_formatter)?;
        }
        Ok(())
    }

    fn draw_ylabel<DB, F>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        axis: &Axis,
        seq: &SequenceRange,
        label_formatter: F,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        F: Fn(isize, usize) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        let ticks = Tick::build_vec(
            (0, 0),
            seq.virtual_range(),
            Direction::Up,
            Direction::Left,
            axis,
            &self.app.y_axis_appearance,
            label_formatter,
        );
        let height = ticks.last().unwrap().tick_start.1.unsigned_abs();
        let layout = Layout(LayoutElem::Horizontal(vec![
            LayoutElem::new_rect_with_id(0, height, "seq_names"),
            LayoutElem::new_rect_with_id(self.app.y_seq_name_setback, height, "ticks"),
            LayoutElem::new_rect_with_id(self.app.y_axis_appearance.axis_thickness, height, "axis"),
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
        backend.draw_text(seq.virtual_name(), style, shift((x, y - height as i32 / 2)))?;
        Ok(())
    }

    fn draw_ylabels<DB, F>(
        &self,
        pos: (i32, i32),
        backend: &mut DB,
        axis: &Axis,
        sorted_planes: &SortedDotPlanes,
        label_formatter: F,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        DB: DrawingBackend,
        F: Fn(isize, usize) -> String,
    {
        let shift = |(x, y): (i32, i32)| (pos.0 + x, pos.1 + y);
        for &(y, seq) in sorted_planes.y_seqs.pos_seq_pairs.iter() {
            self.draw_ylabel(shift((0, -(y as i32))), backend, axis, seq, &label_formatter)?;
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

        let sorted_planes = SortedDotPlanes::new(self, self.app.spacer_thickness);
        let (width, height) = sorted_planes.get_dim();
        let layout = Layout(LayoutElem::Margined {
            margin: LayoutMargin::new(self.app.y_label_area_size, 0, 0, self.app.x_label_area_size),
            center: Box::new(LayoutElem::new_rect(width, height)),
        });
        let axis = Axis::new(self.base_per_pixel as u32, self.app.desired_tick_pitch);

        let range = layout.get_range(".center").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomLeft);
        self.draw_tile(shift(pos), backend, &sorted_planes)?;

        let range = layout.get_range(".left").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::BottomRight);
        self.draw_ylabels(shift(pos), backend, &axis, &sorted_planes, |i, u| {
            format!("{:.1}", i as f64 / u as f64)
        })?;

        let range = layout.get_range(".bottom").unwrap();
        let pos = range.get_relative_pos(RectAnchor::TopLeft, RectAnchor::TopLeft);
        self.draw_xlabels(shift(pos), backend, &axis, &sorted_planes, |i, u| {
            format!("{:.1}", i as f64 / u as f64)
        })?;
        Ok(())
    }
}
