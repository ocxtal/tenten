use crate::seq::Sequence;
use anyhow::Result;
use std::ops::Range;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::DrawingErrorKind;

#[derive(Copy, Clone, Debug)]
pub struct ColorMap {
    pub palette: [RGBColor; 2],
    pub max_density: f64,
    pub min_density: f64,
}

impl ColorMap {
    fn to_picker(self, base_per_pixel: f64) -> ColorPicker {
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

#[derive(Debug, Default)]
pub struct DotPlane {
    cnt: Vec<[u32; 2]>,
    rrange: Range<usize>,
    qrange: Range<usize>,
    width: usize,
    height: usize,
    base_per_pixel: usize,
    picker: ColorPicker,
    pair_id: usize,
}

impl DotPlane {
    fn new(r: &Sequence, q: &Sequence, base_per_pixel: usize, color_map: &ColorMap, pair_id: usize) -> DotPlane {
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
            pair_id,
        }
    }

    fn append_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
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

    pub fn count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a DotPlane<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0)) // always anchored at the top left corner
    }
}

impl<DB> Drawable<DB> for DotPlane<'_>
where
    DB: DrawingBackend,
{
    fn draw<I>(&self, pos: I, backend: &mut DB, _: (u32, u32)) -> Result<(), DrawingErrorKind<DB::ErrorType>>
    where
        I: Iterator<Item = (i32, i32)>,
    {
        let mut pos = pos;
        let pos = pos.next().unwrap();
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
