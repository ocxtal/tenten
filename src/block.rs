// @file plotter.rs
// @author Hajime Suzuki
// @brief dotplot plane data structure

use crate::seq::Seq;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

#[derive(Debug)]
pub struct BlockTile {
    hpx: Vec<u32>,
    vpx: Vec<u32>,
    rseq: Vec<Seq>,
    qseq: Vec<Seq>,
    blocks: Vec<Block>,
}

impl BlockTile {
    pub fn horizontal_pixels(&self) -> &[u32] {
        &self.hpx
    }

    pub fn vertical_pixels(&self) -> &[u32] {
        &self.vpx
    }

    pub fn horizontal_seqs(&self) -> &[Seq] {
        &self.rseq
    }

    pub fn vertical_seqs(&self) -> &[Seq] {
        &self.qseq
    }

    // pub fn horizontal_blocks(&self) -> usize {
    //     self.rseq.len()
    // }

    // pub fn vertical_blocks(&self) -> usize {
    //     self.qseq.len()
    // }

    pub fn get_row(&self, row: usize) -> &[Block] {
        let start = row * self.rseq.len();
        let end = start + self.rseq.len();
        &self.blocks[start..end]
    }

    pub fn count(&self) -> usize {
        self.blocks.iter().map(|x| x.count()).sum::<usize>()
    }

    pub fn base_per_pixel(&self) -> usize {
        self.blocks[0].base_per_pixel
    }
}

#[derive(Debug, Default)]
pub struct Block {
    pub cnt: Vec<[u32; 2]>,
    pub rrange: Range<usize>,
    pub qrange: Range<usize>,
    pub width: usize,
    pub height: usize,
    pub base_per_pixel: usize,
    pair_id: usize,
}

impl Block {
    pub fn new(r: &Seq, q: &Seq, base_per_pixel: usize, pair_id: usize) -> Block {
        let width = r.range.len().div_ceil(base_per_pixel);
        let height = q.range.len().div_ceil(base_per_pixel);
        Block {
            cnt: vec![[0, 0]; width * height],
            rrange: r.range.clone(),
            qrange: q.range.clone(),
            width,
            height,
            base_per_pixel,
            pair_id,
        }
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

    pub fn count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
    }
}

#[derive(Debug)]
pub struct BlockBin {
    pub rseq: Vec<Seq>,
    pub qseq: Vec<Seq>,
    rdedup: HashSet<String>,
    qdedup: HashSet<String>,
    rmap: HashMap<String, Vec<usize>>,
    qmap: HashMap<String, Vec<usize>>,
    blocks: Vec<Block>,
    cmap: HashMap<usize, usize>,
    base_per_pixel: usize,
    tot_size: usize,
}

impl BlockBin {
    pub fn new(rseq: &[Seq], qseq: &[Seq], base_per_pixel: usize) -> BlockBin {
        log::debug!("BlockBin created");
        let mut bin = BlockBin {
            rseq: Vec::new(),
            qseq: Vec::new(),
            rmap: HashMap::new(),
            qmap: HashMap::new(),
            rdedup: HashSet::new(),
            qdedup: HashSet::new(),
            blocks: Vec::new(),
            cmap: HashMap::new(),
            base_per_pixel,
            tot_size: 0,
        };
        for r in rseq {
            bin.add_reference(r);
        }
        for q in qseq {
            bin.add_query(q);
        }
        bin
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
            let pair_id = (qid << 32) | rid;
            self.cmap.insert(pair_id, self.blocks.len());

            let block = Block::new(r, q, self.base_per_pixel, pair_id);
            self.tot_size += block.cnt.len();
            self.blocks.push(block);
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
            let pair_id = (qid << 32) | rid;
            self.cmap.insert(pair_id, self.blocks.len());

            let block = Block::new(r, q, self.base_per_pixel, pair_id);
            self.tot_size += block.cnt.len();
            self.blocks.push(block);
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
                    let pair_id = (qid << 32) | rid;
                    if let Some(&cid) = self.cmap.get(&pair_id) {
                        self.blocks[cid].append_seed(rpos, qpos, is_rev);
                    }
                }
            }
        }
    }

    pub fn split(mut self) -> Vec<BlockBin> {
        let mut v = Vec::new();
        for (rid, rseq) in self.rseq.iter().enumerate() {
            for (qid, qseq) in self.qseq.iter().enumerate() {
                let pair_id = (qid << 32) | rid;
                let cid = *self.cmap.get(&pair_id).unwrap();
                let mut block = std::mem::take(&mut self.blocks[cid]);
                block.pair_id = 0;

                v.push(BlockBin {
                    rseq: vec![rseq.clone()],
                    qseq: vec![qseq.clone()],
                    rdedup: HashSet::from([rseq.to_string()]),
                    qdedup: HashSet::from([qseq.to_string()]),
                    rmap: [(rseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    qmap: [(qseq.name.clone(), vec![0])].into_iter().collect::<HashMap<_, _>>(),
                    blocks: vec![block],
                    cmap: [(0, 0)].into_iter().collect::<HashMap<_, _>>(),
                    base_per_pixel: self.base_per_pixel,
                    tot_size: 0,
                })
            }
        }
        v
    }

    pub fn into_tile(self) -> BlockTile {
        let mut blocks = self.blocks;
        blocks.sort_by(|a, b| a.pair_id.cmp(&b.pair_id));

        let mut hpx = Vec::new();
        for block in blocks.iter().take(self.rseq.len()) {
            hpx.push(block.width as u32);
        }

        let mut vpx = Vec::new();
        for block_chunk in blocks.chunks(self.rseq.len()) {
            vpx.push(block_chunk[0].height as u32);
        }

        BlockTile {
            hpx,
            vpx,
            rseq: self.rseq,
            qseq: self.qseq,
            blocks,
        }
    }
}
