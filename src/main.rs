mod eval;
mod parser;
mod plotter;
mod seq;

use crate::eval::parse_usize;
use crate::parser::{SeedParser, SeedToken};
use crate::plotter::BlockBin;
use crate::seq::{load_range, RangeFormat, Seq};
use clap::{CommandFactory, Parser};
use std::collections::HashMap;
use std::io::{BufRead, Read};
use std::path::Path;
use std::process::{Child, Command, Stdio};

#[derive(Debug, Parser)]
#[command(version)]
pub struct Args {
    #[clap(help = "inputs")]
    pub inputs: Vec<String>,

    #[clap(short = 'P', long, help = "seed generator template")]
    pub seed_generator: Option<String>,

    #[clap(short = 'O', long, help = "use stdout of seed generator, instead of stderr")]
    pub use_stdout: bool,

    #[clap(short = 'f', long, help = "create directory if missing")]
    pub create_missing_dir: bool,

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
        let id = if let Some(cnt) = self.cnt.get_mut(tag) {
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
    #[allow(dead_code)]
    child: Child,
    output: Box<dyn Read>,
}

impl SeedGen {
    fn new(gen: &str, inputs: &[String], use_stdout: bool) -> SeedGen {
        let mut gen = gen.to_string();
        let mut consumed = vec![false; inputs.len()];
        for i in 0..inputs.len() {
            let pat = format!("{{{i}}}");
            if gen.contains(&pat) {
                gen = gen.replacen(&pat, &inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if consumed[i] {
                continue;
            }
            if gen.contains("{}") {
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
        log::info!("executing seed generator: {gen}");

        let cmd = gen.split(" ").collect::<Vec<_>>();
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
        SeedGen { child, output }
    }
}

impl Read for SeedGen {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.output.read(buf)
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

fn ensure_dir(name: &str) {
    let name = Path::new(name);
    let dir = name.parent().unwrap();
    if !dir.exists() {
        std::fs::create_dir_all(dir).unwrap();
    }
}

fn main() {
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let args = Args::parse();
    if args.inputs.is_empty() {
        Args::command().print_help().unwrap();
        return;
    }
    print_args(&std::env::args().collect::<Vec<_>>());

    let file: Box<dyn Read> = if let Some(gen) = &args.seed_generator {
        Box::new(SeedGen::new(gen, &args.inputs, args.use_stdout))
    } else {
        assert!(args.inputs.len() == 1);
        Box::new(std::fs::File::open(&args.inputs[0]).unwrap())
    };
    let file = std::io::BufReader::new(file);

    let rseq = args
        .reference
        .as_ref()
        .map_or_else(Vec::new, |x| load_range(x, &args.reference_format).unwrap());
    let qseq = args
        .query
        .as_ref()
        .map_or_else(Vec::new, |x| load_range(x, &args.query_format).unwrap());
    log::debug!("reference range: {rseq:?}");
    log::debug!("query range: {qseq:?}");
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
                    if args.create_missing_dir {
                        ensure_dir(&name);
                    }
                    b2.plot(&name, args.count_per_seed as f64, args.scale).unwrap();
                }
            }
        } else if b2.count() >= args.min_count {
            let name = name_gen.gen("");
            if args.create_missing_dir {
                ensure_dir(&name);
            }
            b2.plot(&name, args.count_per_seed as f64, args.scale).unwrap();
        }
    };

    let mut parser = SeedParser::new(file.lines(), args.swap_axes);
    while let Some(token) = parser.next() {
        match token {
            SeedToken::NewReference(r) => {
                if args.split_plot && bin.has_plane() {
                    flush(&mut bin);
                }
                if rseq.is_empty() {
                    bin.add_reference(&r);
                }
            }
            SeedToken::NewQuery(q) => {
                if args.split_plot && bin.has_plane() {
                    flush(&mut bin);
                }
                if qseq.is_empty() {
                    bin.add_query(&q);
                }
            }
            SeedToken::Seed(rname, rpos, is_rev, qname, qpos) => {
                bin.append_seed(&rname, rpos, is_rev, &qname, qpos);
            }
        }
    }
    flush(&mut bin);
}
