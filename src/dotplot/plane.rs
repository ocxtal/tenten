use crate::dotplot::color::{
    AnnotationColorMap, AnnotationColorPicker, ColorMode, ColorPicker, DensityColorMap, DensityColorPicker, DirectionMode,
    StainedGlassColorPicker,
};
use crate::dotplot::line::LineStripe;
use crate::dotplot::sequence::SequenceRange;
use crate::dotplot::{Density, LINE_DENSITY_SCALE};
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::ops::Range;

#[derive(Debug, Default)]
pub struct DotPlane {
    cnt: Vec<[u32; 2]>,
    seed_count: usize,
    seed_ops: SeedOps,
    stripe: Option<LineStripe>,
    query_length: usize,
    query_on_x: bool,
    chain_cnt: Option<Vec<[u32; 2]>>,
    rrange: Range<usize>,
    qrange: Range<usize>,
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) base_per_pixel: usize,
    color_map: DensityColorMap,
    density: Density,
    chain_color_map: Option<DensityColorMap>,
    annot: Option<DotPlaneAnnotation>,
    pub(crate) pair_id: usize,
}

#[derive(Copy, Clone, Debug)]
struct SeedOps {
    append: fn(&mut DotPlane, usize, usize, bool),
    finish: fn(&mut DotPlane),
}

impl Default for SeedOps {
    fn default() -> Self {
        Self {
            append: DotPlane::count_seed,
            finish: DotPlane::finish_pixel,
        }
    }
}

#[derive(Debug)]
struct DotPlaneAnnotation {
    rannot: Vec<SequenceRange>,
    qannot: Vec<SequenceRange>,
    picker: AnnotationColorPicker,
}

impl DotPlaneAnnotation {
    fn swap_axes(&self) -> DotPlaneAnnotation {
        DotPlaneAnnotation {
            rannot: self.qannot.clone(),
            qannot: self.rannot.clone(),
            picker: self.picker.clone(),
        }
    }
}

impl DotPlane {
    pub fn new(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        query_length: usize,
        query_on_x: bool,
        density: Density,
    ) -> DotPlane {
        Self::with_pair_id(r, q, base_per_pixel, color_map, 0, query_length, query_on_x, density)
    }

    pub fn with_chain(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        chain_color_map: &DensityColorMap,
        query_length: usize,
        query_on_x: bool,
        density: Density,
    ) -> DotPlane {
        Self::with_pair_id_and_chain(
            r,
            q,
            base_per_pixel,
            color_map,
            chain_color_map,
            0,
            query_length,
            query_on_x,
            density,
        )
    }

    pub fn with_pair_id(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        pair_id: usize,
        query_length: usize,
        query_on_x: bool,
        density: Density,
    ) -> DotPlane {
        let width = r.range.len().div_ceil(base_per_pixel);
        let height = q.range.len().div_ceil(base_per_pixel);
        let query_range = if query_on_x { &r.range } else { &q.range };
        assert!(query_range.end <= query_length);
        let (seed_ops, stripe) = match density {
            Density::Pixel => (SeedOps::default(), None),
            Density::Line { bandwidth } => {
                assert!(query_range.len() <= i32::MAX as usize && base_per_pixel <= i32::MAX as usize);
                (
                    SeedOps {
                        append: Self::buffer_seed,
                        finish: Self::finish_line,
                    },
                    Some(LineStripe::new(bandwidth)),
                )
            }
        };
        DotPlane {
            cnt: vec![[0, 0]; width * height],
            seed_count: 0,
            seed_ops,
            stripe,
            query_length,
            query_on_x,
            chain_cnt: None,
            rrange: r.range.clone(),
            qrange: q.range.clone(),
            width,
            height,
            base_per_pixel,
            color_map: *color_map,
            density,
            chain_color_map: None,
            annot: None,
            pair_id,
        }
    }

    pub fn with_pair_id_and_chain(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        chain_color_map: &DensityColorMap,
        pair_id: usize,
        query_length: usize,
        query_on_x: bool,
        density: Density,
    ) -> DotPlane {
        let mut plane = Self::with_pair_id(r, q, base_per_pixel, color_map, pair_id, query_length, query_on_x, density);
        plane.chain_cnt = Some(vec![[0, 0]; plane.cnt.len()]);
        plane.chain_color_map = Some(*chain_color_map);
        plane
    }

    pub fn swap_axes(&self) -> DotPlane {
        assert!(self.stripe.as_ref().is_none_or(|stripe| stripe.diagonals.is_empty()));
        let mut cnt = vec![[0, 0]; self.height * self.width];
        transpose::transpose(&self.cnt, &mut cnt, self.width, self.height);
        let chain_cnt = self.chain_cnt.as_ref().map(|src| {
            let mut dst = vec![[0, 0]; self.height * self.width];
            transpose::transpose(src, &mut dst, self.width, self.height);
            dst
        });

        let pair_id = self.pair_id >> 32 | self.pair_id << 32;
        DotPlane {
            cnt,
            seed_count: self.seed_count,
            seed_ops: self.seed_ops,
            stripe: self.stripe.clone(),
            query_length: self.query_length,
            query_on_x: !self.query_on_x,
            chain_cnt,
            rrange: self.qrange.clone(),
            qrange: self.rrange.clone(),
            width: self.height,
            height: self.width,
            base_per_pixel: self.base_per_pixel,
            color_map: self.color_map,
            density: self.density,
            chain_color_map: self.chain_color_map,
            annot: self.annot.as_ref().map(|x| x.swap_axes()),
            pair_id,
        }
    }

    pub fn add_annotation(&mut self, r: &[SequenceRange], q: &[SequenceRange], color_map: &AnnotationColorMap) {
        self.annot = Some(DotPlaneAnnotation {
            rannot: r.to_vec(),
            qannot: q.to_vec(),
            picker: color_map.to_picker(),
        });
    }

    pub fn append_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        let Some((rpos, qpos)) = self.seed_position(rpos, qpos, is_rev) else {
            return;
        };
        (self.seed_ops.append)(self, rpos, qpos, is_rev);
        self.seed_count += 1;
    }

    pub fn append_chain_anchor(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        if self.chain_cnt.is_none() {
            return;
        }
        let Some((rpos, qpos)) = self.seed_position(rpos, qpos, is_rev) else {
            return;
        };
        let chain_cnt = self.chain_cnt.as_mut().unwrap();
        let rpos = rpos / self.base_per_pixel;
        let qpos = qpos / self.base_per_pixel;
        chain_cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    pub fn finish_seeds(&mut self) {
        (self.seed_ops.finish)(self);
    }

    pub fn preprocess_counts(&mut self) {
        if self.color_map.direction_mode == DirectionMode::Max {
            for cnt in &mut self.cnt {
                *cnt = [cnt[0].max(cnt[1]), 0];
            }
        }
    }

    fn draw_counts<P: ColorPicker, DB: DrawingBackend>(
        &self,
        counts: &[[u32; 2]],
        pickers: &[P],
        pos: (i32, i32),
        backend: &mut DB,
    ) -> Result<(), DrawingErrorKind<DB::ErrorType>> {
        for (y, line) in counts.chunks(self.width).rev().enumerate() {
            for (x, cnt) in line.iter().enumerate() {
                for (count, picker) in cnt.iter().zip(pickers) {
                    backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), picker.get_color(*count as f64).color())?;
                }
            }
        }
        Ok(())
    }

    pub fn get_seed_count(&self) -> usize {
        self.seed_count
    }

    pub fn bytes(&self) -> usize {
        let seed_bytes = self.cnt.len() * std::mem::size_of::<[u32; 2]>();
        let chain_bytes = self
            .chain_cnt
            .as_ref()
            .map(|cnt| cnt.len() * std::mem::size_of::<[u32; 2]>())
            .unwrap_or(0);
        let stripe_bytes = self
            .stripe
            .as_ref()
            .map_or(0, |stripe| stripe.diagonals.capacity() * std::mem::size_of::<i32>());
        seed_bytes + chain_bytes + stripe_bytes
    }

    fn seed_position(&self, rpos: usize, qpos: usize, is_rev: bool) -> Option<(usize, usize)> {
        let (tpos, qpos, trange, qrange) = if self.query_on_x {
            (qpos, rpos, &self.qrange, &self.rrange)
        } else {
            (rpos, qpos, &self.rrange, &self.qrange)
        };
        if !trange.contains(&tpos) {
            return None;
        }
        assert!(qpos < self.query_length);
        let qpos = if is_rev { self.query_length - 1 - qpos } else { qpos };
        if !qrange.contains(&qpos) {
            return None;
        }
        let tpos = tpos - trange.start;
        let qpos = qpos - qrange.start;
        Some(if self.query_on_x { (qpos, tpos) } else { (tpos, qpos) })
    }

    fn count_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        let rpos = rpos / self.base_per_pixel;
        let qpos = qpos / self.base_per_pixel;
        self.cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    fn finish_pixel(&mut self) {}

    fn buffer_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        let (tpos, qpos, query_len) = if self.query_on_x {
            (qpos, rpos, self.rrange.len())
        } else {
            (rpos, qpos, self.qrange.len())
        };
        let index = tpos / self.base_per_pixel;
        let stripe = self.stripe.as_mut().unwrap();
        if let Some(last) = stripe.last {
            if last.0 == is_rev {
                assert!(tpos >= last.1, "seeds must be target-sorted within each strand");
            } else {
                assert!(!stripe.changed_strand, "strand groups must be contiguous");
                stripe.changed_strand = true;
            }
            if last.0 != is_rev || stripe.index != index {
                self.flush_stripe();
            }
        }
        // Diagonals use strand-oriented query coordinates and a stripe-local target origin.
        let qpos = if is_rev { query_len - 1 - qpos } else { qpos };
        let diagonal = qpos as i32 - (tpos % self.base_per_pixel) as i32;
        let stripe = self.stripe.as_mut().unwrap();
        stripe.index = index;
        stripe.last = Some((is_rev, tpos));
        stripe.diagonals.push(diagonal);
    }

    fn finish_line(&mut self) {
        self.flush_stripe();
        let stripe = self.stripe.as_mut().unwrap();
        stripe.diagonals = Vec::new();
        stripe.last = None;
        stripe.changed_strand = false;
    }

    fn flush_stripe(&mut self) {
        let stripe = self.stripe.as_mut().unwrap();
        if stripe.diagonals.is_empty() {
            return;
        }
        let (target_len, query_len) = if self.query_on_x {
            (self.qrange.len(), self.rrange.len())
        } else {
            (self.rrange.len(), self.qrange.len())
        };
        let target_len = self.base_per_pixel.min(target_len - stripe.index * self.base_per_pixel);
        let index = stripe.index;
        let is_rev = stripe.last.unwrap().0;
        stripe.flush(target_len, query_len, |diagonal, density| {
            let density = (density * 1000.0 * LINE_DENSITY_SCALE).round();
            assert!(density >= 0.0 && density <= u32::MAX as f64);
            let density = density as u32;
            let interval = LineStripe::interval(diagonal, target_len, query_len);
            let start = (interval.start as i64 + diagonal as i64) as usize;
            let end = (interval.end as i64 + diagonal as i64) as usize;
            // Diagonals join seed centers; reflect the interval's edges for reverse paths.
            let (start, end) = if is_rev {
                (query_len - end, query_len - start)
            } else {
                (start, end)
            };
            for qpos in start / self.base_per_pixel..end.div_ceil(self.base_per_pixel) {
                let (x, y) = if self.query_on_x { (qpos, index) } else { (index, qpos) };
                let cell = &mut self.cnt[y * self.width + x][is_rev as usize];
                *cell = (*cell).max(density);
            }
        });
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a DotPlane {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0)) // always anchored at the top left corner
    }
}

impl<DB> Drawable<DB> for DotPlane
where
    DB: DrawingBackend,
{
    fn draw<I>(&self, pos: I, backend: &mut DB, _: (u32, u32)) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        I: Iterator<Item = (i32, i32)>,
    {
        assert!(self.stripe.as_ref().is_none_or(|stripe| stripe.diagonals.is_empty()));
        if self.width == 0 || self.height == 0 {
            return Ok(());
        }

        let mut pos = pos;
        let pos = pos.next().unwrap();

        // first draw the annotations
        if let Some(annot) = &self.annot {
            for r in annot.rannot.iter() {
                let cr = if let Some(name) = r.annotation.as_ref() {
                    annot.picker.get_color(name).color()
                } else {
                    annot.picker.default_color().color()
                };
                let start = r.range.start / self.base_per_pixel;
                let end = r.range.end / self.base_per_pixel;
                backend.draw_rect(
                    (pos.0 + start as i32, pos.1),
                    (pos.0 + end as i32, pos.1 + self.height as i32),
                    &cr,
                    true,
                )?;
            }
            for q in annot.qannot.iter() {
                let cr = if let Some(name) = q.annotation.as_ref() {
                    annot.picker.get_color(name).color()
                } else {
                    annot.picker.default_color().color()
                };
                let start = (self.qrange.end - q.range.start) / self.base_per_pixel;
                let end = (self.qrange.end - q.range.end) / self.base_per_pixel;
                backend.draw_rect(
                    (pos.0, pos.1 + start as i32),
                    (pos.0 + self.width as i32, pos.1 + end as i32),
                    &cr,
                    true,
                )?;
            }
        }

        let channels = self.color_map.direction_mode.channels();
        match self.color_map.color_mode {
            ColorMode::Default => {
                let pickers = self
                    .color_map
                    .palette
                    .map(|color| DensityColorPicker::new(&self.color_map, self.density, color));
                self.draw_counts(&self.cnt, &pickers[..channels], pos, backend)?;
            }
            ColorMode::StainedGlass => {
                let picker = StainedGlassColorPicker::new(&self.color_map, self.density);
                let pickers = [picker; 2];
                self.draw_counts(&self.cnt, &pickers[..channels], pos, backend)?;
            }
        }

        if let (Some(chain_cnt), Some(map)) = (&self.chain_cnt, &self.chain_color_map) {
            let pickers = map.palette.map(|color| DensityColorPicker::new(map, Density::Pixel, color));
            self.draw_counts(chain_cnt, &pickers, pos, backend)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn seq(name: &str, len: usize) -> SequenceRange {
        SequenceRange {
            name: name.to_string(),
            range: 0..len,
            annotation: None,
            virtual_name: None,
            virtual_start: None,
        }
    }

    fn color_map() -> DensityColorMap {
        DensityColorMap {
            color_mode: ColorMode::Default,
            direction_mode: DirectionMode::Separate,
            palette: [RGBColor(255, 0, 64), RGBColor(0, 64, 255)],
            max_density: 400.0,
            min_density: 0.1,
        }
    }

    fn chain_color_map() -> DensityColorMap {
        DensityColorMap {
            color_mode: ColorMode::Default,
            direction_mode: DirectionMode::Separate,
            palette: [RGBColor(0, 0, 0), RGBColor(0, 0, 0)],
            max_density: 400.0,
            min_density: 0.1,
        }
    }

    #[test]
    fn seed_only_constructor_does_not_allocate_chain_counts() {
        let plane = DotPlane::with_pair_id(&seq("r", 100), &seq("q", 100), 10, &color_map(), 7, 100, false, Density::Pixel);
        assert!(plane.chain_cnt.is_none());
        assert!(plane.chain_color_map.is_none());
    }

    #[test]
    fn chain_constructor_allocates_chain_counts() {
        let plane = DotPlane::with_pair_id_and_chain(
            &seq("r", 100),
            &seq("q", 100),
            10,
            &color_map(),
            &chain_color_map(),
            7,
            100,
            false,
            Density::Pixel,
        );
        assert_eq!(plane.chain_cnt.as_ref().unwrap().len(), 100);
        assert!(plane.chain_color_map.is_some());
    }

    #[test]
    fn append_chain_anchor_counts_forward_and_reverse_separately() {
        let mut plane = DotPlane::with_chain(
            &seq("r", 100),
            &seq("q", 100),
            10,
            &color_map(),
            &chain_color_map(),
            100,
            false,
            Density::Pixel,
        );

        plane.append_chain_anchor(25, 35, false);
        plane.append_chain_anchor(25, 35, true);

        let chain_cnt = plane.chain_cnt.as_ref().unwrap();
        assert_eq!(chain_cnt[3 * 10 + 2], [1, 0]);
        assert_eq!(chain_cnt[6 * 10 + 2], [0, 1]);
    }

    #[test]
    fn append_chain_anchor_is_noop_without_chain_counts() {
        let mut plane = DotPlane::new(&seq("r", 100), &seq("q", 100), 10, &color_map(), 100, false, Density::Pixel);
        plane.append_chain_anchor(25, 35, false);
        assert!(plane.chain_cnt.is_none());
    }

    #[test]
    fn swap_axes_transposes_chain_counts() {
        let mut plane = DotPlane::with_chain(
            &seq("r", 100),
            &seq("q", 50),
            10,
            &color_map(),
            &chain_color_map(),
            50,
            false,
            Density::Pixel,
        );
        plane.append_chain_anchor(25, 35, false);

        let swapped = plane.swap_axes();
        let chain_cnt = swapped.chain_cnt.as_ref().unwrap();

        assert_eq!(swapped.width, 5);
        assert_eq!(swapped.height, 10);
        assert_eq!(chain_cnt[2 * 5 + 3], [1, 0]);
    }
}
