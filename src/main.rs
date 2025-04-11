mod eval;
mod parser;
mod plotter;
mod seq;

use crate::eval::parse_usize;
use crate::parser::{SeedParser, SeedToken};
use crate::plotter::BlockBin;
use crate::seq::{RangeFormat, Seq, load_range};
use clap::Parser;
use std::io::{BufRead, Read};
use std::path::Path;
use std::process::{Child, Command, Stdio};

#[derive(Clone, Debug, Parser)]
#[command(version)]
pub struct Args {
    #[clap(help = "reference or seed file")]
    pub reference: String,

    #[clap(help = "query")]
    pub query: Option<String>,

    #[clap(short = 'P', long, help = "seed generator command template")]
    pub seed_generator: Option<String>,

    #[clap(short = 'O', long, help = "use stdout of seed generator, instead of stderr")]
    pub use_stdout: bool,

    #[clap(short = 'f', long, help = "create directory if missing")]
    pub create_missing_dir: bool,

    #[clap(short = 'r', long, help = "bases per pixel", default_value = "100", value_parser = parse_usize)]
    pub base_per_pixel: usize,

    #[clap(short = 'w', long, help = "maximum width in pixel", default_value = "6400", value_parser = parse_usize)]
    pub max_width: usize,

    #[clap(short = 'h', long, help = "maximum height in pixel", default_value = "6400", value_parser = parse_usize)]
    pub max_height: usize,

    #[clap(
        short = 'D',
        long,
        help = "density of seeds (#per 1kbp square) corresponds to 50% heatmap scale",
        default_value = "100.0"
    )]
    pub density: f64,

    #[clap(
        short = 'M',
        long,
        help = "do not output image if seed density is less than this value",
        default_value = "0"
    )]
    pub min_density: f64,

    #[clap(short = 'c', long, help = "count per seed", default_value = "1", value_parser = parse_usize)]
    pub count_per_seed: usize,

    #[clap(short = 's', long, help = "scaling factor", default_value = "25")]
    pub scale: f64,

    #[clap(
        short = 'm',
        long,
        help = "do not output image if #seed is less than this value",
        default_value = "0"
    )]
    pub min_count: usize,

    #[clap(short = 'p', long, help = "split plot", default_value = "false")]
    pub split_plot: bool,

    #[clap(short = 'x', long, help = "swap x/y axes when parsing the seed stream", default_value = "false")]
    pub parse_swap: bool,

    #[clap(short = 'X', long, help = "swap x/y axes when plotting", default_value = "false")]
    pub plot_swap: bool,

    #[clap(
        short = 'r',
        long,
        help = "crop reference sequences by the ranges in this file. in fasta, bed, or \"chr7:6000000-6300000\""
    )]
    pub reference_range: Option<String>,

    #[clap(
        short = 'R',
        long,
        help = "force treat the reference range file in a specific format",
        default_value = "infer"
    )]
    pub reference_range_format: RangeFormat,

    #[clap(
        short = 'q',
        long,
        help = "crop query sequences by the ranges in this file. in fasta, bed, or \"chr7:6000000-6300000\""
    )]
    pub query_range: Option<String>,

    #[clap(
        short = 'Q',
        long,
        help = "force treat the query range file in a specific format",
        default_value = "infer"
    )]
    pub query_range_format: RangeFormat,

    #[clap(short = 'o', long, help = "Output filename (prefix if split plot)", default_value = "out.png")]
    pub output: String,

    #[clap(
        short = 'T',
        long,
        help = "print to terminal (encoded to iTerm2 image format)",
        default_value = "false"
    )]
    pub output_iterm2: bool,
}

struct SeedGeneratorCommand {
    #[allow(dead_code)]
    child: Child,
    output: Box<dyn Read>,
}

impl SeedGeneratorCommand {
    fn new(cmd: &str, inputs: &[&str], use_stdout: bool) -> SeedGeneratorCommand {
        let mut cmd = cmd.to_string();
        let mut consumed = vec![false; inputs.len()];
        for i in 0..inputs.len() {
            let pat = format!("{{{i}}}");
            if cmd.contains(&pat) {
                cmd = cmd.replacen(&pat, &inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if consumed[i] {
                continue;
            }
            if cmd.contains("{}") {
                cmd = cmd.replacen("{}", &inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if !consumed[i] {
                cmd = format!("{0} {1}", cmd, inputs[i]);
                consumed[i] = true;
            }
        }
        assert!(consumed.iter().all(|&x| x));
        log::info!("executing seed generator: {cmd}");

        let cmd = cmd.split(" ").collect::<Vec<_>>();
        let (child, output) = if use_stdout {
            let mut child = Command::new(cmd[0])
                .args(&cmd[1..])
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .spawn()
                .unwrap();
            let output: Box<dyn Read> = Box::new(child.stdout.take().unwrap());
            (child, output)
        } else {
            let mut child = Command::new(cmd[0])
                .args(&cmd[1..])
                .stdout(Stdio::null())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap();
            let output: Box<dyn Read> = Box::new(child.stderr.take().unwrap());
            (child, output)
        };
        SeedGeneratorCommand { child, output }
    }
}

impl Read for SeedGeneratorCommand {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.output.read(buf)
    }
}

struct Context {
    basename: String,
    rseq: Vec<Seq>,
    qseq: Vec<Seq>,
    bin: BlockBin,
    args: Args,
}

impl Context {
    fn new(args: &Args) -> Self {
        let rseq = args
            .reference_range
            .as_ref()
            .map_or_else(Vec::new, |x| load_range(x, &args.reference_range_format).unwrap());
        let qseq = args
            .query_range
            .as_ref()
            .map_or_else(Vec::new, |x| load_range(x, &args.query_range_format).unwrap());
        log::debug!("reference ranges: {rseq:?}");
        log::debug!("query ranges: {qseq:?}");
        let (rseq, qseq) = if args.parse_swap { (qseq, rseq) } else { (rseq, qseq) };

        let basename = if let Some(basename) = args.output.strip_suffix(".png") {
            basename.to_string()
        } else {
            args.output.clone()
        };

        Context {
            basename,
            rseq,
            qseq,
            bin: BlockBin::new(args.base_per_pixel),
            args: args.clone(),
        }
    }

    fn create_name(&self, bin: &BlockBin) -> String {
        if self.args.split_plot {
            format!(
                "{}.{}.{}.png",
                &self.basename,
                bin.rseq[0].to_path_string(),
                bin.qseq[0].to_path_string()
            )
        } else {
            format!("{}.png", &self.basename)
        }
    }

    fn ensure_dir(&self, name: &str) {
        if !self.args.create_missing_dir {
            return;
        }
        let name = Path::new(name);
        let dir = name.parent().unwrap();
        if !dir.exists() {
            std::fs::create_dir_all(dir).unwrap();
        }
    }

    fn plot(&self, bin: &BlockBin) {
        let name = self.create_name(bin);

        let count = bin.count();
        if count < self.args.min_count {
            log::info!("skip {} as count {} < {}", &name, count, self.args.min_count);
            return;
        }
        self.ensure_dir(&name);
        bin.plot(&name, self.args.count_per_seed as f64, self.args.scale).unwrap();
    }

    fn pop_bin(&mut self) -> BlockBin {
        let mut bin = BlockBin::new(self.args.base_per_pixel);
        for r in &self.rseq {
            bin.add_reference(r);
        }
        for q in &self.qseq {
            bin.add_query(q);
        }
        std::mem::replace(&mut self.bin, bin)
    }

    fn flush(&mut self) {
        let bin = self.pop_bin();
        if self.args.split_plot {
            let split = bin.split();
            for bin in &split {
                self.plot(bin);
            }
        } else {
            self.plot(&bin);
        }
    }

    fn add_reference(&mut self, r: &Seq) {
        if self.args.split_plot && self.bin.has_plane() {
            self.flush();
        }
        if self.rseq.is_empty() {
            self.bin.add_reference(r);
        }
    }

    fn add_query(&mut self, q: &Seq) {
        if self.args.split_plot && self.bin.has_plane() {
            self.flush();
        }
        if self.qseq.is_empty() {
            self.bin.add_query(q);
        }
    }

    fn append_seed(&mut self, rname: &str, rpos: usize, is_rev: bool, qname: &str, qpos: usize) {
        self.bin.append_seed(rname, rpos, is_rev, qname, qpos);
    }
}

fn print_args(args: &[String]) {
    let args = args
        .iter()
        .map(|x| if x.contains(' ') { format!("\"{x}\"") } else { x.to_string() })
        .collect::<Vec<_>>();
    let args = args.join(" ");
    log::info!("args: {args}");
}

fn main() {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let args = Args::parse();
    print_args(&std::env::args().collect::<Vec<_>>());

    let file: Box<dyn Read> = if let Some(query) = &args.query {
        let seed_generator = args.seed_generator.as_ref().unwrap();
        Box::new(SeedGeneratorCommand::new(
            seed_generator,
            &[&args.reference, query],
            args.use_stdout,
        ))
    } else {
        Box::new(std::fs::File::open(&args.reference).unwrap())
    };
    let file = std::io::BufReader::new(file);

    let mut ctx = Context::new(&args);
    let mut parser = SeedParser::new(file.lines(), args.parse_swap);
    while let Some(token) = parser.next() {
        match token {
            SeedToken::NewReference(r) => ctx.add_reference(&r),
            SeedToken::NewQuery(q) => ctx.add_query(&q),
            SeedToken::Seed(rname, rpos, is_rev, qname, qpos) => ctx.append_seed(&rname, rpos, is_rev, &qname, qpos),
        }
    }
    ctx.flush();
}
