// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plotter

use crate::BlockTile;
use anyhow::{Result, anyhow};
use plotters::prelude::*;

struct Breakpoints {
    v: Vec<u32>, // first element is always zero; ignored when splitting plot plane
}

impl Breakpoints {
    fn from_pixels(pixels: &[u32]) -> Breakpoints {
        let mut acc = 0;
        let mut v = vec![0];
        for p in pixels {
            v.push(acc);
            acc += p;
        }
        Breakpoints { v }
    }

    fn as_slice(&self) -> &[u32] {
        &self.v[1..]
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

    fn to_margined(&self, left: u32, right: u32) -> Breakpoints {
        let mut b = Breakpoints { v: vec![0, left] };
        b.extend_slice(&self.v);
        b.extend_slice(&[0, right]);
        b
    }

    fn to_smashed(&self) -> Breakpoints {
        let last = self.v.last().unwrap();
        Breakpoints { v: vec![0, *last] }
    }

    fn pixels(&self) -> u32 {
        *self.v.last().unwrap()
    }
}

pub struct Plotter {
    margin: usize,
    x_label_area_size: usize,
    y_label_area_size: usize,
    count_per_seed: f64,
    scale: f64,
}

impl Plotter {
    pub fn new(count_per_seed: f64, scale: f64) -> Plotter {
        Plotter {
            margin: 20,
            x_label_area_size: 10,
            y_label_area_size: 40,
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

    pub fn plot(&self, name: &str, tile: &BlockTile) -> Result<()> {
        let block_pixels = (tile.rpixels(), tile.qpixels());

        let dotplanes = (Breakpoints::from_pixels(&block_pixels.0), Breakpoints::from_pixels(&block_pixels.1));
        let with_labels = (
            dotplanes.0.to_smashed().to_margined(self.x_label_area_size as u32, 0),
            dotplanes.1.to_smashed().to_margined(self.y_label_area_size as u32, 0),
        );
        let with_margin = (
            with_labels.0.to_margined(self.margin as u32, self.margin as u32),
            with_labels.1.to_margined(self.margin as u32, self.margin as u32),
        );
        let pixels = (with_margin.0.pixels(), with_margin.0.pixels());
        if pixels.0 >= 65536 || pixels.1 >= 65536 {
            return Err(anyhow!("plotting area too large: {} x {}", pixels.0, pixels.1));
        }
        let root = BitMapBackend::new(&name, (pixels.0, pixels.1)).into_drawing_area();
        root.fill(&WHITE).unwrap();

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
