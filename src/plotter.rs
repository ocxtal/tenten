// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use crate::block::{Block, BlockTile};
use crate::seq::Seq;
use anyhow::{Result, anyhow};
use plotters::coord::Shift;
use plotters::element::{Drawable, PointCollection};
use plotters::prelude::*;
use plotters_backend::DrawingErrorKind;
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

    fn extend_slice(&mut self, v: &[u32]) {
        let offset = *self.v.last().unwrap();
        for b in v.iter().skip(1) {
            self.v.push(offset + b);
        }
    }

    fn extend(&mut self, other: &Breakpoints) {
        self.extend_slice(&other.v);
    }

    fn to_margined(&self, left: Option<u32>, right: Option<u32>) -> Breakpoints {
        let mut b = Breakpoints { v: vec![0] };
        if let Some(left) = left {
            b.extend_slice(&[0, left]);
        }
        b.extend_slice(&self.v);
        if let Some(right) = right {
            b.extend_slice(&[0, right]);
        }
        b
    }

    fn to_smashed(&self) -> Breakpoints {
        let last = self.v.last().unwrap();
        Breakpoints { v: vec![0, *last] }
    }

    fn reverse(&mut self) {
        dbg!(&self.v);
        let mut acc = 0;
        let mut v = vec![0];
        for w in self.v.windows(2).rev() {
            acc += w[1] - w[0];
            v.push(acc);
        }
        self.v = v;
        dbg!(&self.v);
    }

    fn pixels(&self) -> u32 {
        *self.v.last().unwrap()
    }

    fn segments(&self) -> usize {
        self.v.len() - 1
    }
}

enum LayoutElem {
    Rect(String, u32, u32),
    Horizontal(String, Box<Vec<LayoutElem>>),
    Vertical(String, Box<Vec<LayoutElem>>),
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
}

// wrapper of Block to make drawable on Plotter's DrawingArea
#[derive(Debug)]
struct DrawableBlock<'a>(&'a Block);

impl<'a> DrawableBlock<'a> {
    fn new(block: &'a Block) -> DrawableBlock<'a> {
        DrawableBlock(block)
    }
}

impl<'a> PointCollection<'a, (i32, i32)> for &'a DrawableBlock<'_> {
    type Point = &'a (i32, i32);
    type IntoIter = std::iter::Once<&'a (i32, i32)>;

    fn point_iter(self) -> Self::IntoIter {
        std::iter::once(&(0, 0))
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
        backend.draw_rect(pos, (pos.0 + 10, pos.1 + 12), &RED, false)?;
        Ok(())
    }
}

pub struct Plotter {
    margin: usize,
    spacer: usize,
    x_label_area_size: usize,
    y_label_area_size: usize,
    count_per_seed: f64,
    scale: f64,
}

impl Plotter {
    pub fn new(count_per_seed: f64, scale: f64) -> Plotter {
        Plotter {
            margin: 20,
            spacer: 1,
            x_label_area_size: 20,
            y_label_area_size: 40,
            // tick: 3,
            count_per_seed,
            scale,
        }
    }

    // fn calc_boundaries(&self, len: &[usize]) -> Vec<usize> {
    //     let mut pos = 0;
    //     let mut boundaries = vec![0];
    //     for l in len {
    //         pos += l;

    //         let boundary = pos.div_ceil(self.base_per_pixel);
    //         pos = (boundary + 1) * self.base_per_pixel; // 1px for bar
    //         boundaries.push(boundary);
    //     }
    //     boundaries
    // }

    fn draw_origin(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>) {
        area.fill(&YELLOW).unwrap();

        let style = ShapeStyle {
            color: BLACK.into(),
            filled: false,
            stroke_width: 1,
        };
        let path = PathElement::new(&[(0, 0), (0, self.y_label_area_size as i32)], style);
        area.draw(&path).unwrap();

        // area.draw_pixel((self.y_label_area_size as i32 - 1, 0), &BLACK).unwrap();
    }

    fn draw_xlabel(area: &DrawingArea<BitMapBackend<'_>, Shift>, seq: &Seq) {
        // let style = ("sans-serif", 20, &BLACK).into_text_style(area);
        // area.draw_text(label, &style, (0, 0)).unwrap();
        // area.fill(&RED).unwrap();
    }

    fn draw_ylabel(area: &DrawingArea<BitMapBackend<'_>, Shift>, seq: &Seq) {
        // let style = ("sans-serif", 20, &BLACK).into_text_style(area);
        // area.draw_text(label, &style, (0, 0)).unwrap();
        // area.fill(&GREEN).unwrap();
    }

    fn cnt_to_color(&self, min: (u8, u8, u8), max: (u8, u8, u8), val: u32) -> (u8, u8, u8) {
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

    fn draw_block(&self, area: &DrawingArea<BitMapBackend<'_>, Shift>, block: &Block) -> Result<()> {
        for (y, line) in block.cnt.chunks(block.width).rev().enumerate() {
            for (x, cnt) in line.iter().enumerate() {
                let c0 = self.cnt_to_color((255, 255, 255), (255, 0, 64), cnt[0]);
                let c1 = self.cnt_to_color((255, 255, 255), (0, 64, 255), cnt[1]);
                let c = std::cmp::min(c0, c1);
                area.draw_pixel((x as i32, y as i32 + 1), &RGBColor(c.0, c.1, c.2))?;
            }
        }
        Ok(())
    }

    pub fn plot(&self, name: &str, tile: &BlockTile) -> Result<()> {
        let block_pixels = (
            tile.horizontal_pixels().iter().map(|&x| x + self.spacer as u32).collect::<Vec<_>>(),
            tile.vertical_pixels().iter().map(|&x| x + self.spacer as u32).collect::<Vec<_>>(),
        );
        let block_brks = (Breakpoints::from_pixels(&block_pixels.0), Breakpoints::from_pixels(&block_pixels.1));

        let layout = LayoutElem::Margined(
            "margin".to_string(),
            Box::new(LayoutElem::Margined(
                "label".to_string(),
                Box::new(LayoutElem::Rect("blocks".to_string(), block_brks.0.pixels(), block_brks.1.pixels())),
                (self.y_label_area_size as u32, 0),
                (0, self.x_label_area_size as u32),
            )),
            (self.margin as u32, self.margin as u32),
            (self.margin as u32, self.margin as u32),
        );
        let areas = StructuredDrawingArea::from_layout(&layout, name)?;
        dbg!(&areas.index);

        let ylabel = areas.get_area(".margin[center].label[left]")?;
        ylabel.fill(&RED).unwrap();

        let block_area = areas.get_area(".margin[center].label[center]")?;
        let block_areas = block_area.split_by_breakpoints(block_brks.0.as_slice(), block_brks.1.as_slice());
        for (i, areas) in block_areas.chunks(block_brks.0.segments()).rev().enumerate() {
            let blocks = tile.get_row(i);
            for (area, block) in areas.iter().zip(blocks.iter()) {
                area.draw(&DrawableBlock(block)).unwrap();
            }
        }

        Ok(())
    }

    pub fn plot2(&self, name: &str, tile: &BlockTile) -> Result<()> {
        // let block_pixels = (
        //     tile.rpixels().iter().map(|&x| x + self.spacer as u32).collect::<Vec<_>>(),
        //     tile.qpixels().iter().map(|&x| x + self.spacer as u32).collect::<Vec<_>>(),
        // );

        // let mut block_brks = (Breakpoints::from_pixels(&block_pixels.0), Breakpoints::from_pixels(&block_pixels.1));
        // let mut label_brks = (
        //     block_brks.0.to_smashed().to_margined(Some(self.y_label_area_size as u32), None),
        //     block_brks.1.to_smashed().to_margined(Some(self.x_label_area_size as u32), None),
        // );
        // let mut margin_brks = (
        //     label_brks.0.to_smashed().to_margined(Some(self.margin as u32), Some(self.margin as u32)),
        //     label_brks.1.to_smashed().to_margined(Some(self.margin as u32), Some(self.margin as u32)),
        // );
        // let pixels = (margin_brks.0.pixels(), margin_brks.0.pixels());
        // if pixels.0 >= 65536 || pixels.1 >= 65536 {
        //     return Err(anyhow!("plotting area too large: {} x {}", pixels.0, pixels.1));
        // }

        // block_brks.1.reverse();
        // label_brks.1.reverse();
        // margin_brks.1.reverse();

        // dbg!(&block_pixels);
        // dbg!(&block_brks);
        // dbg!(&label_brks);
        // dbg!(&margin_brks);

        // let root = BitMapBackend::new(&name, (pixels.0, pixels.1)).into_drawing_area();
        // root.fill(&WHITE).unwrap();

        // let margin_areas = root.split_by_breakpoints(margin_brks.0.as_slice(), margin_brks.1.as_slice());
        // for (i, a) in margin_areas.iter().enumerate() {
        //     log::debug!("margin area {}: {:?}", i, a.get_pixel_range());
        // }
        // let label_areas = margin_areas[4].split_by_breakpoints(label_brks.0.as_slice(), label_brks.1.as_slice());
        // for (i, a) in label_areas.iter().enumerate() {
        //     log::debug!("label area {}: {:?}", i, a.get_pixel_range());
        // }
        // self.draw_origin(&label_areas[2]);

        // let xlabel_areas = label_areas[3].split_by_breakpoints(block_brks.0.as_slice(), &[0u32][1..]);
        // for (a, s) in xlabel_areas.iter().zip(tile.rseq.iter()) {
        //     log::debug!("x: seq: {:?} label at {:?}", s, a.get_pixel_range());
        //     Self::draw_xlabel(a, s);
        // }

        // let ylabel_areas = label_areas[0].split_by_breakpoints(&[0u32][1..], block_brks.1.as_slice());
        // for (a, s) in ylabel_areas.iter().zip(tile.qseq.iter()) {
        //     log::debug!("y: seq: {:?} label at {:?}", s, a.get_pixel_range());
        //     Self::draw_ylabel(a, s);
        // }

        // let block_areas = label_areas[1].split_by_breakpoints(block_brks.0.as_slice(), block_brks.1.as_slice());
        // for (ac, bc) in block_areas.chunks(tile.rseq.len()).rev().zip(tile.blocks.chunks(tile.rseq.len())) {
        //     for (a, b) in ac.iter().zip(bc.iter()) {
        //         self.draw_block(a, b)?;
        //     }
        // }

        // let rlen = tile.rseq.iter().map(|x| x.range.len()).collect::<Vec<_>>();
        // let rbnd = self.calc_boundaries(&rlen);

        // let qlen = tile.qseq.iter().map(|x| x.range.len()).collect::<Vec<_>>();
        // let qbnd = self.calc_boundaries(&qlen);

        // let margin = 20;
        // let x_label_area_size = 10;
        // let y_label_area_size = 40;
        // let rstart = *rbnd.first().unwrap() + 1;
        // let qstart = *qbnd.first().unwrap() + 1;
        // let plot_width = *rbnd.last().unwrap() - rstart;
        // let plot_height = *qbnd.last().unwrap() - qstart;

        // if plot_width >= 65536 || plot_height >= 65536 {
        //     return Err(anyhow!("plotting area too large: {} x {}", plot_width, plot_height));
        // }
        // let root = BitMapBackend::new(
        //     &name,
        //     (
        //         2 * margin + y_label_area_size + plot_width as u32,
        //         2 * margin + x_label_area_size + plot_height as u32,
        //     ),
        // )
        // .into_drawing_area();
        // root.fill(&WHITE).unwrap();

        // let mut chart = ChartBuilder::on(&root)
        //     .margin(margin)
        //     .x_label_area_size(x_label_area_size)
        //     .y_label_area_size(y_label_area_size)
        //     .build_cartesian_2d(
        //         0.0..(plot_width * self.base_per_pixel) as f64,
        //         0.0..(plot_height * self.base_per_pixel) as f64,
        //     )?;

        // chart
        //     .configure_mesh()
        //     .disable_x_mesh()
        //     .disable_y_mesh()
        //     .x_label_formatter(&|x| format!("{:.1?}", x / 1000.0))
        //     .y_label_formatter(&|y| format!("{:.1?}", y / 1000.0))
        //     .draw()?;

        // let plotting_area = chart.plotting_area();
        // let blend = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
        //     let r = (min.0 as u32 * (256 - val) + max.0 as u32 * val) / 256;
        //     let g = (min.1 as u32 * (256 - val) + max.1 as u32 * val) / 256;
        //     let b = (min.2 as u32 * (256 - val) + max.2 as u32 * val) / 256;

        //     (r as u8, g as u8, b as u8)
        // };
        // let to_color = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
        //     let occ = val as f64 * count_per_seed;
        //     let occ = scale * occ.log2();
        //     let occ = (occ as i32).clamp(0, 256) as u32;

        //     blend(min, max, occ)
        // };

        // for rid in 0..rbnd.len() - 1 {
        //     for qid in 0..qbnd.len() - 1 {
        //         let pair_id = (qid << 32) | rid;
        //         if let Some(&cid) = self.cmap.get(&pair_id) {
        //             let block = &self.blocks[cid];
        //             for (i, c) in block.cnt.iter().enumerate() {
        //                 let x = i % block.width + rbnd[rid];
        //                 let y = i / block.width + qbnd[qid];
        //                 let c0 = to_color((255, 255, 255), (255, 0, 64), c[0]);
        //                 let c1 = to_color((255, 255, 255), (0, 64, 255), c[1]);
        //                 let c = std::cmp::min(c0, c1);
        //                 plotting_area.draw_pixel(
        //                     ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
        //                     &RGBColor(c.0, c.1, c.2),
        //                 )?;
        //             }
        //         }
        //     }
        // }

        // // draw boundaries
        // for &x in &rbnd {
        //     for y in 0..=plot_height {
        //         plotting_area.draw_pixel(
        //             ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
        //             &RGBColor(192, 208, 192),
        //         )?;
        //     }
        // }
        // for &y in &qbnd {
        //     for x in 0..=plot_width {
        //         plotting_area.draw_pixel(
        //             ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
        //             &RGBColor(192, 208, 192),
        //         )?;
        //     }
        // }

        // // let style = ("sans-serif", 20, &BLACK).into_text_style(&root);
        // // root.draw_text("aaaa", &style, (100, 100)).unwrap();

        // root.present().unwrap();
        // log::info!("plotted: {:?} with query {:?} and reference {:?}", name, self.qseq, self.rseq);
        Ok(())
    }
}
