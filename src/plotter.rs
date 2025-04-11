// @file block.rs
// @author Hajime Suzuki
// @brief dotplot plane data structure

use crate::Seq;
use anyhow::{anyhow, Result};
use plotters::prelude::*;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct Block {
    cnt: Vec<[u32; 2]>,
    rbase: usize,
    qbase: usize,
    width: usize,
    height: usize,
    base_per_pixel: usize,
}

impl Block {
    pub fn new(r: &Seq, q: &Seq, base_per_pixel: usize) -> Block {
        let width = r.range.len().div_ceil(base_per_pixel);
        let height = q.range.len().div_ceil(base_per_pixel);
        Block {
            cnt: vec![[0, 0]; width * height],
            rbase: r.range.start,
            qbase: q.range.start,
            width,
            height,
            base_per_pixel,
        }
    }

    pub fn append_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
        if rpos < self.rbase || qpos < self.qbase {
            return;
        }
        let rpos = (rpos - self.rbase) / self.base_per_pixel;
        let qpos = (qpos - self.qbase) / self.base_per_pixel;
        if rpos >= self.width || qpos >= self.height {
            return;
        }
        self.cnt[qpos * self.width + rpos][is_rev as usize] += 1;
    }

    pub fn count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
    }
}

pub struct BlockBin {
    pub rseq: Vec<Seq>,
    pub qseq: Vec<Seq>,
    rdedup: HashSet<String>,
    qdedup: HashSet<String>,
    rmap: HashMap<String, Vec<usize>>,
    qmap: HashMap<String, Vec<usize>>,
    cnts: Vec<Block>,
    cmap: HashMap<usize, usize>,
    base_per_pixel: usize,
    tot_size: usize,
}

impl BlockBin {
    pub fn new(base_per_pixel: usize) -> BlockBin {
        log::debug!("BlockBin created");
        BlockBin {
            rseq: Vec::new(),
            qseq: Vec::new(),
            rmap: HashMap::new(),
            qmap: HashMap::new(),
            rdedup: HashSet::new(),
            qdedup: HashSet::new(),
            cnts: Vec::new(),
            cmap: HashMap::new(),
            base_per_pixel,
            tot_size: 0,
        }
    }

    pub fn has_plane(&self) -> bool {
        !self.rseq.is_empty() && !self.qseq.is_empty()
    }

    pub fn add_reference(&mut self, r: &Seq) {
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
            let pair = (qid << 32) | rid;
            self.cmap.insert(pair, self.cnts.len());

            let block = Block::new(r, q, self.base_per_pixel);
            self.tot_size += block.cnt.len();
            self.cnts.push(block);
        }
        log::debug!("reference added: {:?}, {:}", &r.name, self.tot_size);
    }

    pub fn add_query(&mut self, q: &Seq) {
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
            let pair = (qid << 32) | rid;
            self.cmap.insert(pair, self.cnts.len());

            let block = Block::new(r, q, self.base_per_pixel);
            self.tot_size += block.cnt.len();
            self.cnts.push(block);
        }
        log::debug!("query added: {:?}, {:}", &q.name, self.tot_size);
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
                    let pair = (qid << 32) | rid;
                    if let Some(&cid) = self.cmap.get(&pair) {
                        self.cnts[cid].append_seed(rpos, qpos, is_rev);
                    }
                }
            }
        }
    }

    pub fn count(&self) -> usize {
        self.cnts.iter().map(|x| x.count()).sum::<usize>()
    }

    pub fn split(mut self) -> Vec<BlockBin> {
        let mut v = Vec::new();
        for (rid, rseq) in self.rseq.iter().enumerate() {
            for (qid, qseq) in self.qseq.iter().enumerate() {
                let pair = (qid << 32) | rid;
                let cid = *self.cmap.get(&pair).unwrap();
                let block = std::mem::take(&mut self.cnts[cid]);

                v.push(BlockBin {
                    rseq: vec![rseq.clone()],
                    qseq: vec![qseq.clone()],
                    rdedup: HashSet::from([rseq.to_string()]),
                    qdedup: HashSet::from([qseq.to_string()]),
                    rmap: [(rseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    qmap: [(qseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    cnts: vec![block],
                    cmap: [(0, 0)].into_iter().collect::<HashMap<_, _>>(),
                    base_per_pixel: self.base_per_pixel,
                    tot_size: 0,
                })
            }
        }
        v
    }

    fn calc_boundaries(&self, len: &[usize]) -> Vec<usize> {
        let mut pos = 0;
        let mut boundaries = vec![0];
        for l in len {
            pos += l;

            let boundary = pos.div_ceil(self.base_per_pixel);
            pos = (boundary + 1) * self.base_per_pixel; // 1px for bar
            boundaries.push(boundary);
        }
        boundaries
    }

    pub fn plot(&self, name: &str, count_per_seed: f64, scale: f64) -> Result<()> {
        let rlen = self.rseq.iter().map(|x| x.range.len()).collect::<Vec<_>>();
        let rbnd = self.calc_boundaries(&rlen);

        let qlen = self.qseq.iter().map(|x| x.range.len()).collect::<Vec<_>>();
        let qbnd = self.calc_boundaries(&qlen);

        let margin = 20;
        let x_label_area_size = 10;
        let y_label_area_size = 40;
        let rstart = *rbnd.first().unwrap() + 1;
        let qstart = *qbnd.first().unwrap() + 1;
        let plot_width = *rbnd.last().unwrap() - rstart;
        let plot_height = *qbnd.last().unwrap() - qstart;

        if plot_width >= 65536 || plot_height >= 65536 {
            return Err(anyhow!("plotting area too large: {} x {}", plot_width, plot_height));
        }
        let root = BitMapBackend::new(
            &name,
            (
                2 * margin + y_label_area_size + plot_width as u32,
                2 * margin + x_label_area_size + plot_height as u32,
            ),
        )
        .into_drawing_area();
        root.fill(&WHITE).unwrap();

        let mut chart = ChartBuilder::on(&root)
            .margin(margin)
            .x_label_area_size(x_label_area_size)
            .y_label_area_size(y_label_area_size)
            .build_cartesian_2d(
                0.0..(plot_width * self.base_per_pixel) as f64,
                0.0..(plot_height * self.base_per_pixel) as f64,
            )?;

        chart
            .configure_mesh()
            .disable_x_mesh()
            .disable_y_mesh()
            .x_label_formatter(&|x| format!("{:.1?}", x / 1000.0))
            .y_label_formatter(&|y| format!("{:.1?}", y / 1000.0))
            .draw()?;

        let plotting_area = chart.plotting_area();
        // let range = plotting_area.get_pixel_range();

        let blend = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
            let r = (min.0 as u32 * (256 - val) + max.0 as u32 * val) / 256;
            let g = (min.1 as u32 * (256 - val) + max.1 as u32 * val) / 256;
            let b = (min.2 as u32 * (256 - val) + max.2 as u32 * val) / 256;

            (r as u8, g as u8, b as u8)
        };
        let to_color = |min: (u8, u8, u8), max: (u8, u8, u8), val: u32| -> (u8, u8, u8) {
            let occ = val as f64 * count_per_seed;
            let occ = scale * occ.log2();
            let occ = (occ as i32).clamp(0, 256) as u32;

            blend(min, max, occ)
        };

        for rid in 0..rbnd.len() - 1 {
            for qid in 0..qbnd.len() - 1 {
                let pair = (qid << 32) | rid;
                if let Some(&cid) = self.cmap.get(&pair) {
                    let block = &self.cnts[cid];
                    for (i, c) in block.cnt.iter().enumerate() {
                        let x = i % block.width + rbnd[rid];
                        let y = i / block.width + qbnd[qid];
                        let c0 = to_color((255, 255, 255), (255, 0, 64), c[0]);
                        let c1 = to_color((255, 255, 255), (0, 64, 255), c[1]);
                        let c = std::cmp::min(c0, c1);
                        plotting_area.draw_pixel(
                            ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
                            &RGBColor(c.0, c.1, c.2),
                        )?;
                    }
                }
            }
        }

        // draw boundaries
        for &x in &rbnd {
            for y in 0..plot_height {
                plotting_area.draw_pixel(
                    ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
                    &RGBColor(192, 208, 192),
                )?;
            }
        }
        for &y in &qbnd {
            for x in 0..plot_width {
                plotting_area.draw_pixel(
                    ((x * self.base_per_pixel) as f64, (y * self.base_per_pixel) as f64),
                    &RGBColor(192, 208, 192),
                )?;
            }
        }

        let style = ("sans-serif", 20, &BLACK).into_text_style(&root);
        root.draw_text("aaaa", &style, (100, 100)).unwrap();

        root.present().unwrap();
        log::info!("plotted: {:?} with query {:?} and reference {:?}", name, self.qseq, self.rseq);
        Ok(())
    }
}
