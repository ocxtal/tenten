use crate::dotplot::color::{AnnotationColorMap, AnnotationColorPicker, DensityColorMap, DensityColorPicker};
use crate::dotplot::sequence::SequenceRange;
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::ops::Range;

#[derive(Debug, Default)]
pub struct DotPlane {
    cnt: Vec<[u32; 2]>,
    chain_cnt: Option<Vec<[u32; 2]>>,
    rrange: Range<usize>,
    qrange: Range<usize>,
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) base_per_pixel: usize,
    picker: DensityColorPicker,
    chain_picker: Option<DensityColorPicker>,
    annot: Option<DotPlaneAnnotation>,
    pub(crate) pair_id: usize,
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
    pub fn new(r: &SequenceRange, q: &SequenceRange, base_per_pixel: usize, color_map: &DensityColorMap) -> DotPlane {
        Self::with_pair_id(r, q, base_per_pixel, color_map, 0)
    }

    pub fn with_chain(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        chain_color_map: &DensityColorMap,
    ) -> DotPlane {
        Self::with_pair_id_and_chain(r, q, base_per_pixel, color_map, chain_color_map, 0)
    }

    pub fn with_pair_id(
        r: &SequenceRange,
        q: &SequenceRange,
        base_per_pixel: usize,
        color_map: &DensityColorMap,
        pair_id: usize,
    ) -> DotPlane {
        let width = r.range.len().div_ceil(base_per_pixel);
        let height = q.range.len().div_ceil(base_per_pixel);
        DotPlane {
            cnt: vec![[0, 0]; width * height],
            chain_cnt: None,
            rrange: r.range.clone(),
            qrange: q.range.clone(),
            width,
            height,
            base_per_pixel,
            picker: color_map.to_picker(base_per_pixel as f64),
            chain_picker: None,
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
    ) -> DotPlane {
        let width = r.range.len().div_ceil(base_per_pixel);
        let height = q.range.len().div_ceil(base_per_pixel);
        DotPlane {
            cnt: vec![[0, 0]; width * height],
            chain_cnt: Some(vec![[0, 0]; width * height]),
            rrange: r.range.clone(),
            qrange: q.range.clone(),
            width,
            height,
            base_per_pixel,
            picker: color_map.to_picker(base_per_pixel as f64),
            chain_picker: Some(chain_color_map.to_picker(base_per_pixel as f64)),
            annot: None,
            pair_id,
        }
    }

    pub fn swap_axes(&self) -> DotPlane {
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
            chain_cnt,
            rrange: self.qrange.clone(),
            qrange: self.rrange.clone(),
            width: self.height,
            height: self.width,
            base_per_pixel: self.base_per_pixel,
            picker: self.picker.clone(),
            chain_picker: self.chain_picker.clone(),
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
        if !self.rrange.contains(&rpos) || !self.qrange.contains(&qpos) {
            return;
        }
        let rpos = (rpos - self.rrange.start) / self.base_per_pixel;
        let qpos = if is_rev {
            (self.qrange.end - qpos) / self.base_per_pixel
        } else {
            (qpos - self.qrange.start) / self.base_per_pixel
        };
        debug_assert!(rpos < self.width && qpos < self.height);

        self.cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    pub fn append_chain_anchor(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        let Some(chain_cnt) = self.chain_cnt.as_mut() else {
            return;
        };
        if !self.rrange.contains(&rpos) || !self.qrange.contains(&qpos) {
            return;
        }

        let rpos = (rpos - self.rrange.start) / self.base_per_pixel;
        let qpos = if is_rev {
            (self.qrange.end - qpos) / self.base_per_pixel
        } else {
            (qpos - self.qrange.start) / self.base_per_pixel
        };
        debug_assert!(rpos < self.width && qpos < self.height);

        chain_cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    pub fn get_seed_count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
    }

    pub fn bytes(&self) -> usize {
        let seed_bytes = self.cnt.len() * std::mem::size_of::<[u32; 2]>();
        let chain_bytes = self
            .chain_cnt
            .as_ref()
            .map(|cnt| cnt.len() * std::mem::size_of::<[u32; 2]>())
            .unwrap_or(0);
        seed_bytes + chain_bytes
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

        // then plot dots
        for (y, line) in self.cnt.chunks(self.width).rev().enumerate() {
            for (x, cnt) in line.iter().enumerate() {
                let cf = self.picker.get_color(0, cnt[0]).color();
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cf)?;

                let cr = self.picker.get_color(1, cnt[1]).color();
                backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cr)?;
            }
        }

        if let (Some(chain_cnt), Some(chain_picker)) = (&self.chain_cnt, &self.chain_picker) {
            for (y, line) in chain_cnt.chunks(self.width).rev().enumerate() {
                for (x, cnt) in line.iter().enumerate() {
                    let cf = chain_picker.get_color(0, cnt[0]).color();
                    backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cf)?;

                    let cr = chain_picker.get_color(1, cnt[1]).color();
                    backend.draw_pixel((pos.0 + x as i32, pos.1 + y as i32), cr)?;
                }
            }
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
            palette: [RGBColor(255, 0, 64), RGBColor(0, 64, 255)],
            max_density: 400.0,
            min_density: 0.1,
        }
    }

    fn chain_color_map() -> DensityColorMap {
        DensityColorMap {
            palette: [RGBColor(0, 0, 0), RGBColor(0, 0, 0)],
            max_density: 400.0,
            min_density: 0.1,
        }
    }

    #[test]
    fn seed_only_constructor_does_not_allocate_chain_counts() {
        let plane = DotPlane::with_pair_id(&seq("r", 100), &seq("q", 100), 10, &color_map(), 7);
        assert!(plane.chain_cnt.is_none());
        assert!(plane.chain_picker.is_none());
    }

    #[test]
    fn chain_constructor_allocates_chain_counts() {
        let plane = DotPlane::with_pair_id_and_chain(&seq("r", 100), &seq("q", 100), 10, &color_map(), &chain_color_map(), 7);
        assert_eq!(plane.chain_cnt.as_ref().unwrap().len(), 100);
        assert!(plane.chain_picker.is_some());
    }

    #[test]
    fn append_chain_anchor_counts_forward_and_reverse_separately() {
        let mut plane = DotPlane::with_chain(&seq("r", 100), &seq("q", 100), 10, &color_map(), &chain_color_map());

        plane.append_chain_anchor(25, 35, false);
        plane.append_chain_anchor(25, 35, true);

        let chain_cnt = plane.chain_cnt.as_ref().unwrap();
        assert_eq!(chain_cnt[3 * 10 + 2], [1, 0]);
        assert_eq!(chain_cnt[6 * 10 + 2], [0, 1]);
    }

    #[test]
    fn append_chain_anchor_is_noop_without_chain_counts() {
        let mut plane = DotPlane::new(&seq("r", 100), &seq("q", 100), 10, &color_map());
        plane.append_chain_anchor(25, 35, false);
        assert!(plane.chain_cnt.is_none());
    }

    #[test]
    fn swap_axes_transposes_chain_counts() {
        let mut plane = DotPlane::with_chain(&seq("r", 100), &seq("q", 50), 10, &color_map(), &chain_color_map());
        plane.append_chain_anchor(25, 35, false);

        let swapped = plane.swap_axes();
        let chain_cnt = swapped.chain_cnt.as_ref().unwrap();

        assert_eq!(swapped.width, 5);
        assert_eq!(swapped.height, 10);
        assert_eq!(chain_cnt[2 * 5 + 3], [1, 0]);
    }
}
