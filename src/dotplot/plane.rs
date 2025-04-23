use crate::dotplot::color::{AnnotationColorMap, AnnotationColorPicker, DensityColorMap, DensityColorPicker};
use crate::dotplot::sequence::SequenceRange;
use anyhow::Result;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::{BackendStyle, DrawingErrorKind};
use std::ops::Range;

#[derive(Debug, Default)]
pub struct DotPlane {
    pub(crate) cnt: Vec<[u32; 2]>,
    rrange: Range<usize>,
    qrange: Range<usize>,
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) base_per_pixel: usize,
    picker: DensityColorPicker,
    annot: Option<DotPlaneAnnotation>,
    pub(crate) pair_id: usize,
}

#[derive(Debug)]
struct DotPlaneAnnotation {
    rannot: Vec<SequenceRange>,
    qannot: Vec<SequenceRange>,
    picker: AnnotationColorPicker,
}

impl DotPlane {
    pub fn new(r: &SequenceRange, q: &SequenceRange, base_per_pixel: usize, color_map: &DensityColorMap) -> DotPlane {
        Self::with_pair_id(r, q, base_per_pixel, color_map, 0)
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
            rrange: r.range.clone(),
            qrange: q.range.clone(),
            width,
            height,
            base_per_pixel,
            picker: color_map.to_picker(base_per_pixel as f64),
            annot: None,
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
            (qpos - self.qrange.start - 1) / self.base_per_pixel
        };
        debug_assert!(rpos < self.width && qpos < self.height);

        self.cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    pub fn get_seed_count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
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
        let mut pos = pos;
        let pos = pos.next().unwrap();

        // first draw the annotations
        if let Some(annot) = &self.annot {
            for r in annot.rannot.iter() {
                if let Some(name) = r.annotation.as_ref() {
                    let cr = annot.picker.get_color(name).color();
                    let start = r.range.start / self.base_per_pixel;
                    let end = r.range.end / self.base_per_pixel;
                    backend.draw_rect(
                        (pos.0 + start as i32, pos.1),
                        (pos.0 + end as i32, pos.1 + self.height as i32),
                        &cr,
                        true,
                    )?;
                }
            }
            for q in annot.qannot.iter() {
                if let Some(name) = q.annotation.as_ref() {
                    let cr = annot.picker.get_color(name).color();
                    let start = q.range.start / self.base_per_pixel;
                    let end = q.range.end / self.base_per_pixel;
                    backend.draw_rect(
                        (pos.0, pos.1 + start as i32),
                        (pos.0 + self.width as i32, pos.1 + end as i32),
                        &cr,
                        true,
                    )?;
                }
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
        Ok(())
    }
}
