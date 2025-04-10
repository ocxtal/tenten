mod eval;

use crate::eval::parse_usize;
use anyhow::{anyhow, Result};
use clap::{Parser, ValueEnum};
use plotters::prelude::*;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io::{BufRead, Read};
use std::ops::Range;
use std::process::{Child, Command, Stdio};

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum RangeFormat {
    Fasta,
    Bed,  // chr7\t6000000\t6300000
    Text, // chr7:6000000-6300000
    Infer,
}

#[derive(Debug, Parser)]
pub struct Args {
    #[clap(help = "inputs")]
    pub inputs: Vec<String>,

    #[clap(short = 'P', long, help = "seed generator template")]
    pub seed_generator: Option<String>,

    #[clap(short = 'E', long, help = "use stderr of seed generator, instead of stdout")]
    pub use_stderr: bool,

    #[clap(short = 'w', long, help = "bases per pixel", default_value = "100", value_parser = parse_usize)]
    pub base_per_pixel: usize,

    #[clap(short = 'c', long, help = "count per seed", default_value = "1", value_parser = parse_usize)]
    pub count_per_seed: usize,

    #[clap(short = 's', long, help = "scaling factor", default_value = "25")]
    pub scale: f64,

    #[clap(short = 'm', long, help = "minimum seed count in a plane to plot", default_value = "1000")]
    pub min_count: usize,

    #[clap(short = 'p', long, help = "split plot", default_value = "false")]
    pub split_plot: bool,

    #[clap(short = 'x', long, help = "swap x/y axes", default_value = "false")]
    pub swap_axes: bool,

    #[clap(short = 'r', long, help = "reference range file in fasta, bed, or \"chr7:6000000-6300000\"")]
    pub reference: Option<String>,

    #[clap(
        short = 'R',
        long,
        help = "force treat the reference range file in a specific format",
        default_value = "infer"
    )]
    pub reference_format: RangeFormat,

    #[clap(short = 'q', long, help = "query range file")]
    pub query: Option<String>,

    #[clap(
        short = 'Q',
        long,
        help = "force treat the query range file in a specific format",
        default_value = "infer"
    )]
    pub query_format: RangeFormat,

    #[clap(short, long, help = "Output filename (prefix if split plot)", default_value = "out.png")]
    pub output: String,
}

#[derive(Clone, Default)]
struct Seq {
    pub name: String,
    pub range: Range<usize>,
}

impl fmt::Display for Seq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.name, self.range.start, self.range.end)
    }
}

impl fmt::Debug for Seq {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.name, self.range.start, self.range.end)
    }
}

fn load_range_fasta(file: &str) -> Result<Vec<Seq>> {
    let file = std::fs::File::open(file)?;
    let file = std::io::BufReader::new(file);

    let mut v = Vec::new();
    let (mut name, mut len, mut first) = (None, 0, true);
    for line in file.lines() {
        let line = line?;
        if line.starts_with('>') {
            if let Some(mut name) = name {
                v.push(Seq {
                    name: std::mem::take(&mut name),
                    range: 0..len,
                });
            }

            let cols = line.split_ascii_whitespace().collect::<Vec<_>>();
            assert!(!cols.is_empty() && !cols[0].is_empty());
            (name, len) = (Some(cols[0][1..].to_string()), 0);
        } else {
            if first {
                return Err(anyhow!("failed to parse fasta: {:?}", line.trim()));
            }
            first = false;
            len += line.trim().len();
        }
    }
    Ok(v)
}

fn load_range_bed(file: &str) -> Result<Vec<Seq>> {
    let file = std::fs::File::open(file)?;
    let file = std::io::BufReader::new(file);

    let mut v = Vec::new();
    for line in file.lines() {
        let line = line?;
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        if cols.len() < 3 {
            return Err(anyhow!("failed to parse bed: {:?}", line.trim()));
        }

        let name = cols[0].to_string();
        let start = cols[1].parse::<usize>()?;
        let end = cols[2].parse::<usize>()?;
        v.push(Seq { name, range: start..end })
    }
    Ok(v)
}

fn load_range_text(file: &str) -> Result<Vec<Seq>> {
    let file = std::fs::File::open(file)?;
    let file = std::io::BufReader::new(file);

    let mut v = Vec::new();
    for line in file.lines() {
        let line = line?;
        let cols = line.trim().split(':').collect::<Vec<_>>();
        if cols.len() != 2 {
            return Err(anyhow!("failed to parse range text: {:?}", line.trim()));
        }
        let pos = cols[1].trim().split('-').collect::<Vec<_>>();
        if pos.len() != 2 {
            return Err(anyhow!("failed to parse range text: {:?}", line.trim()));
        }

        let name = cols[0].to_string();
        let start = pos[0].parse::<usize>()?;
        let end = pos[1].parse::<usize>()?;
        v.push(Seq { name, range: start..end })
    }
    Ok(v)
}

fn load_range(file: &str, format: &RangeFormat) -> Result<Vec<Seq>> {
    match format {
        RangeFormat::Fasta => load_range_fasta(file),
        RangeFormat::Bed => load_range_bed(file),
        RangeFormat::Text => load_range_text(file),
        RangeFormat::Infer => {
            if let Ok(r) = load_range_text(file) {
                return Ok(r);
            } else if let Ok(r) = load_range_bed(file) {
                return Ok(r);
            }
            return load_range_fasta(file);
        }
    }
}

struct SeedParser<T>
where
    T: Iterator<Item = std::io::Result<String>>,
{
    it: T,
    swap: bool,
}

#[derive(Debug)]
enum Token {
    NewReference(Seq),
    NewQuery(Seq),
    Seed(String, usize, bool, String, usize),
}

impl<T> SeedParser<T>
where
    T: Iterator<Item = std::io::Result<String>>,
{
    fn new(it: T, swap: bool) -> SeedParser<T> {
        SeedParser { it, swap }
    }

    fn parse_seq(is_query: bool, line: &str) -> Option<Token> {
        let mut s = Seq::default();
        for (i, col) in line.split('\t').enumerate() {
            match i {
                0 => s.name = col.to_string(),
                1 => s.range = 0..col.parse::<usize>().unwrap(),
                2.. => return None,
            }
        }
        if is_query {
            Some(Token::NewQuery(s))
        } else {
            Some(Token::NewReference(s))
        }
    }

    fn parse_seed(&self, line: &str) -> Option<Token> {
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        assert!(cols.len() == 5, "{:?}", line);
        assert!(cols[2] == "-" || cols[2] == "+");

        let rname = cols[0].to_string();
        let rpos = cols[1].parse::<usize>().unwrap();
        let is_rev = cols[2] == "-";
        let qname = cols[3].to_string();
        let qpos = cols[4].parse::<usize>().unwrap();
        let (rname, rpos, qname, qpos) = if self.swap {
            (qname, qpos, rname, rpos)
        } else {
            (rname, rpos, qname, qpos)
        };

        Some(Token::Seed(rname, rpos, is_rev, qname, qpos))
    }

    fn next(&mut self) -> Option<Token> {
        while let Some(line) = self.it.next() {
            let line = line.ok()?;
            let line = line.trim();
            // eprintln!("line: {line}");
            if line.starts_with("[") {
                continue;
            } else if line.starts_with("@") {
                return None;
            } else if line.starts_with("#ref\t") {
                return Self::parse_seq(self.swap, &line[5..]);
            } else if line.starts_with("#query\t") {
                return Self::parse_seq(!self.swap, &line[7..]);
            } else {
                return self.parse_seed(&line);
            }
        }
        None
    }
}

#[derive(Default)]
struct Block {
    cnt: Vec<[u32; 2]>,
    rbase: usize,
    qbase: usize,
    width: usize,
    height: usize,
    base_per_pixel: usize,
}

impl Block {
    fn new(r: &Seq, q: &Seq, base_per_pixel: usize) -> Block {
        let width = (r.range.len() + base_per_pixel - 1) / base_per_pixel;
        let height = (q.range.len() + base_per_pixel - 1) / base_per_pixel;
        Block {
            cnt: vec![[0, 0]; width * height],
            rbase: r.range.start,
            qbase: q.range.start,
            width,
            height,
            base_per_pixel,
        }
    }

    fn append_seed(&mut self, rpos: usize, qpos: usize, is_rev: bool) {
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

    fn count(&self) -> usize {
        self.cnt.iter().map(|x| x[0] as usize + x[1] as usize).sum::<usize>()
    }
}

struct BlockBin {
    rseq: Vec<Seq>,
    qseq: Vec<Seq>,
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
    fn new(base_per_pixel: usize) -> BlockBin {
        eprintln!("BlockBin created");
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

    fn has_plane(&self) -> bool {
        !self.rseq.is_empty() && !self.qseq.is_empty()
    }

    fn add_reference(&mut self, r: &Seq) {
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
        eprintln!("reference added: {:?}, {:}", &r.name, self.tot_size);
    }

    fn add_query(&mut self, q: &Seq) {
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
        eprintln!("query added: {:?}, {:}", &q.name, self.tot_size);
    }

    fn append_seed(&mut self, rname: &str, rpos: usize, is_rev: bool, qname: &str, qpos: usize) {
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

    fn count(&self) -> usize {
        self.cnts.iter().map(|x| x.count()).sum::<usize>()
    }

    fn split(mut self) -> Vec<BlockBin> {
        let mut v = Vec::new();
        for (rid, rseq) in self.rseq.iter().enumerate() {
            for (qid, qseq) in self.qseq.iter().enumerate() {
                let pair = (qid << 32) | rid;
                let cid = *self.cmap.get(&pair).unwrap();
                let block = std::mem::replace(&mut self.cnts[cid], Block::default());

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

            let boundary = (pos + self.base_per_pixel - 1) / self.base_per_pixel;
            pos = (boundary + 1) * self.base_per_pixel; // 1px for bar
            boundaries.push(boundary);
        }
        boundaries
    }

    fn plot(&self, name: &str, count_per_seed: f64, scale: f64) -> Result<()> {
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
            let occ = std::cmp::max(0, std::cmp::min(occ as i32, 256)) as u32;

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

        root.present().unwrap();
        eprintln!("plotted: {:?} with query {:?} and reference {:?}", name, self.qseq, self.rseq);
        Ok(())
    }
}

struct NameGen {
    base: String,
    cnt: HashMap<String, usize>,
}

impl NameGen {
    fn new(base: &str) -> NameGen {
        NameGen {
            base: base.to_string(),
            cnt: HashMap::new(),
        }
    }

    fn gen(&mut self, tag: &str) -> String {
        let id = if let Some(cnt) = self.cnt.get_mut(&tag.to_string()) {
            *cnt += 1;
            *cnt
        } else {
            self.cnt.insert(tag.to_string(), 0);
            0
        };
        if tag.is_empty() {
            format!("{}.{}.png", &self.base, id)
        } else {
            format!("{}.{}.{}.png", &self.base, tag, id)
        }
    }
}

struct SeedGen {
    child: Child,
    output: Box<dyn Read>,
}

impl SeedGen {
    fn new(gen: &str, inputs: &[String], use_stderr: bool) -> SeedGen {
        let mut gen = gen.to_string();
        let mut consumed = vec![false; inputs.len()];
        for i in 0..inputs.len() {
            let pat = format!("{{{i}}}");
            if let Some(_) = gen.find(&pat) {
                gen = gen.replacen(&pat, &inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if consumed[i] {
                continue;
            }
            if let Some(_) = gen.find("{}") {
                gen = gen.replacen("{}", &inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if !consumed[i] {
                gen = format!("{0} {1}", gen, inputs[i]);
                consumed[i] = true;
            }
        }
        assert!(consumed.iter().all(|&x| x));
        eprintln!("cmd: {gen}");

        let cmd = gen.split(" ").collect::<Vec<_>>();
        let (child, output) = if use_stderr {
            let mut child = Command::new(&cmd[0]).args(&cmd[1..]).stdout(Stdio::null()).stderr(Stdio::piped()).spawn().unwrap();
            let output: Box<dyn Read> = Box::new(child.stderr.take().unwrap());
            (child, output)
        } else {
            let mut child = Command::new(&cmd[0]).args(&cmd[1..]).stdout(Stdio::piped()).stderr(Stdio::null()).spawn().unwrap();
            let output: Box<dyn Read> = Box::new(child.stdout.take().unwrap());
            (child, output)
        };
        SeedGen {
            child,
            output,
        }
    }
}

impl Read for SeedGen {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.output.read(buf)

        // let ret = self.output.read(buf);
        // if let Ok(l) = ret {
        //     eprintln!("{}", unsafe { std::str::from_utf8_unchecked(&buf[..l]) });
        //     return Ok(l);
        // }
        // eprintln!("err: {ret:?}");
        // ret
    }
}

fn print_args(args: &[String]) {
    let args = args.iter().map(|x| {
        if let Some(_) = x.find(' ') {
            format!("\"{x}\"")
        } else {
            x.to_string()
        }
    }).collect::<Vec<_>>();
    let args = args.join(" ");
    eprintln!("args: {args}");
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    print_args(&args);

    let args = Args::parse();
    let file: Box<dyn Read> = if let Some(gen) = &args.seed_generator {
        Box::new(SeedGen::new(gen, &args.inputs, args.use_stderr))
    } else {
        assert!(args.inputs.len() == 1);
        Box::new(std::fs::File::open(&args.inputs[0]).unwrap())
    };
    let file = std::io::BufReader::new(file);

    let rseq = args
        .reference
        .as_ref()
        .map_or_else(|| Vec::new(), |x| load_range(x, &args.reference_format).unwrap());
    let qseq = args
        .query
        .as_ref()
        .map_or_else(|| Vec::new(), |x| load_range(x, &args.query_format).unwrap());
    eprintln!("reference range: {rseq:?}");
    eprintln!("query range: {qseq:?}");
    let (rseq, qseq) = if args.swap_axes { (qseq, rseq) } else { (rseq, qseq) };

    let newbin = || {
        let mut bin = BlockBin::new(args.base_per_pixel);
        for r in &rseq {
            bin.add_reference(r);
        }
        for q in &qseq {
            bin.add_query(q);
        }
        bin
    };
    let mut bin = newbin();

    let mut name_gen = NameGen::new(&args.output);
    let mut flush = |b: &mut BlockBin| {
        let b2 = std::mem::replace(b, newbin());
        if args.split_plot {
            let split = b2.split();
            for b2 in &split {
                if b2.count() >= args.min_count {
                    let name = name_gen.gen(&format!("{}.{}", &b2.rseq[0], &b2.qseq[0]));
                    b2.plot(&name, args.count_per_seed as f64, args.scale).unwrap();
                }
            }
        } else {
            if b2.count() >= args.min_count {
                let name = name_gen.gen("");
                b2.plot(&name, args.count_per_seed as f64, args.scale).unwrap();
            }
        }
    };

    let mut parser = SeedParser::new(file.lines(), args.swap_axes);
    while let Some(token) = parser.next() {
        match token {
            Token::NewReference(r) => {
                if args.split_plot && bin.has_plane() {
                    flush(&mut bin);
                }
                if rseq.is_empty() {
                    bin.add_reference(&r);
                }
            }
            Token::NewQuery(q) => {
                if args.split_plot && bin.has_plane() {
                    flush(&mut bin);
                }
                if qseq.is_empty() {
                    bin.add_query(&q);
                }
            }
            Token::Seed(rname, rpos, is_rev, qname, qpos) => {
                bin.append_seed(&rname, rpos, is_rev, &qname, qpos);
            }
        }
    }
    flush(&mut bin);
}
