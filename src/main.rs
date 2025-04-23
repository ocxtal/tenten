mod parser;

use crate::parser::{SeedParser, SeedToken};
use clap::Parser;
use plotters::prelude::*;
use regex::Regex;
use std::collections::HashMap;
use std::io::{BufRead, Read};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use tempfile::NamedTempFile;
use tenten::*;

#[derive(Clone, Debug, Parser)]
#[command(version)]
pub struct Args {
    #[clap(
        help = "Reference sequence (if query is passed) or seeds in minimap2 --print-seeds format",
        name = "REFERENCE or SEED FILE"
    )]
    pub reference: String,

    #[clap(help = "Query sequence")]
    pub query: Option<String>,

    #[clap(
        short = 'P',
        long,
        help = "Seed generator command template",
        default_value = "minimap2 -t1 --print-seeds {0} {1}"
    )]
    pub seed_generator: Option<String>,

    #[clap(short = 'O', long, help = "Use stdout of seed generator, instead of stderr")]
    pub use_stdout: bool,

    #[clap(short = 'b', long, help = "Bases per pixel", default_value = "100")]
    pub base_per_pixel: usize,

    #[clap(
        short = 'D',
        long,
        help = "Density of seeds (#per 1kbp square) that corresponds to 50% heatmap scale",
        default_value = "20.0"
    )]
    pub density: f64,

    #[clap(
        short = 'M',
        long,
        help = "Do not output image if seed density is less than this value",
        default_value = "0.1"
    )]
    pub min_density: f64,

    #[clap(
        short = 'm',
        long,
        help = "Do not output image if #seed is less than this value",
        default_value = "0"
    )]
    pub min_count: usize,

    #[clap(short = 'x', long, help = "Swap reference and query for seed generator", default_value = "false")]
    pub swap_generator: bool,

    #[clap(short = 'X', long, help = "Swap x/y axes of the output plots", default_value = "false")]
    pub swap_plot_axes: bool,

    #[clap(
        short = 'E',
        long,
        help = "Extract sequence range from its name on plotting (in the \"chr7:6000000-6300000\" format)",
        default_value = "false"
    )]
    pub extract_range_from_name: bool,

    #[clap(
        short = 'r',
        long,
        help = "Plot seeds only in the ranges in the file for references. Accepts BED or \"chr7:6000000-6300000\" format."
    )]
    pub reference_range: Option<String>,

    #[clap(
        short = 'R',
        long,
        help = "Force treat the reference range file in a specific format",
        default_value = "infer"
    )]
    pub reference_range_format: RangeFormat,

    #[clap(
        short = 'q',
        long,
        help = "Plot seeds only in the ranges in the file for queries. Accepts BED or \"chr7:6000000-6300000\" format."
    )]
    pub query_range: Option<String>,

    #[clap(
        short = 'Q',
        long,
        help = "Force treat the query range file in a specific format",
        default_value = "infer"
    )]
    pub query_range_format: RangeFormat,

    #[clap(long, help = "Regex to extract labels from sequence name (reference side)")]
    pub reference_label_extractor: Option<String>,

    #[clap(long, help = "Regex to extract labels from sequence name (query side)")]
    pub query_label_extractor: Option<String>,

    #[clap(long, help = "Annotation file for the reference (in BED format)")]
    pub reference_annotation: Option<String>,

    #[clap(long, help = "Annotation file for the query (in BED format)")]
    pub query_annotation: Option<String>,

    // #[clap(short = 'A', long, help = "Annotation color configuration in yaml")]
    // pub annotation_color: Option<String>,
    #[clap(short = 'o', long, help = "Output filename (prefix if split plot)", default_value = "out.png")]
    pub output: String,

    #[clap(short = 'f', long, help = "Create directory if it doesn't exist")]
    pub create_missing_dir: bool,

    #[clap(short = 'p', long, help = "Create plot for each reference/query pair", default_value = "false")]
    pub split_plot: bool,

    #[clap(
        short = 'S',
        long,
        help = "Assume the seed generator output is sorted (reduces memory usage when --split-plot)",
        default_value = "false"
    )]
    pub sorted: bool,

    #[clap(
        short = 'T',
        long,
        help = "Print plot to terminal (encoded to iTerm2 image format)",
        default_value = "false"
    )]
    pub iterm2: bool,

    #[clap(short = 'W', long, help = "Width in characters when printing plot to terminal")]
    pub iterm2_width: Option<usize>,

    #[clap(short = 'H', long, help = "Height in characters when printing plot to terminal")]
    pub iterm2_height: Option<usize>,

    #[clap(long, help = "Suppress logs")]
    pub quiet: bool,
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
                cmd = cmd.replacen(&pat, inputs[i], 1);
                consumed[i] = true;
            }
        }
        for i in 0..inputs.len() {
            if consumed[i] {
                continue;
            }
            if cmd.contains("{}") {
                cmd = cmd.replacen("{}", inputs[i], 1);
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

struct LabelExtractor {
    re: Regex,
}

impl LabelExtractor {
    fn new(re: &str) -> LabelExtractor {
        let re = Regex::new(re).unwrap();
        LabelExtractor { re }
    }

    fn patch_sequence(&self, seq: &mut SequenceRange) {
        if let Some(cap) = self.re.captures(&seq.name) {
            if let Some(start) = cap.name("start") {
                log::debug!("start found: {:?}", start.as_str());
                if let Ok(start) = start.as_str().parse::<isize>() {
                    seq.offset_to_coord_in_plot = start;
                }
            } else if let Some(offset) = cap.name("offset") {
                log::debug!("offset found: {:?}", offset.as_str());
                if let Ok(offset) = offset.as_str().parse::<isize>() {
                    seq.offset_to_coord_in_plot = offset;
                }
            }

            if let Some(name) = cap.name("name") {
                log::debug!("name found: {:?}", name.as_str());
                seq.name_in_plot = Some(name.as_str().to_string());
            }
            let name = seq.name_in_plot();
            let range = seq.range_in_plot();
            log::debug!("sequence patched: {}:{}-{}", name, range.start, range.end);
        }
    }
}

struct Context<'a> {
    basename: String,
    rseq: Vec<SequenceRange>,
    qseq: Vec<SequenceRange>,
    dotplot: DotPlot<'a>,
    dot_color: &'a DensityColorMap,
    rannot: Vec<SequenceRange>,
    qannot: Vec<SequenceRange>,
    annot_color: &'a AnnotationColorMap,
    appearance: &'a DotPlotAppearance<'a>,
    args: Args,
}

impl<'a> Context<'a> {
    fn new(
        args: &Args,
        dot_color: &'a DensityColorMap,
        annot_color: &'a AnnotationColorMap,
        appearance: &'a DotPlotAppearance<'a>,
    ) -> Self {
        let (rseq, qseq) = if let Some(query) = &args.query {
            let rseq = load_sequence_range(&args.reference, RangeFormat::Fasta).unwrap();
            let qseq = load_sequence_range(query, RangeFormat::Fasta).unwrap();
            (rseq, qseq)
        } else {
            (Vec::new(), Vec::new())
        };

        let mut rseq = if let Some(reference_range) = &args.reference_range {
            load_sequence_range(reference_range, args.reference_range_format).unwrap()
        } else {
            rseq
        };
        let mut qseq = if let Some(query_range) = &args.query_range {
            load_sequence_range(query_range, args.query_range_format).unwrap()
        } else {
            qseq
        };

        if let Some(ref re) = args.reference_label_extractor {
            let extractor = LabelExtractor::new(re);
            for seq in &mut rseq {
                extractor.patch_sequence(seq);
            }
        }
        if let Some(ref re) = args.query_label_extractor {
            let extractor = LabelExtractor::new(re);
            for seq in &mut qseq {
                extractor.patch_sequence(seq);
            }
        }

        let (rseq, qseq) = if args.swap_generator { (qseq, rseq) } else { (rseq, qseq) };

        log::debug!("reference ranges: {rseq:?}");
        log::debug!("query ranges: {qseq:?}");

        let basename = if let Some(basename) = args.output.strip_suffix(".png") {
            basename.to_string()
        } else {
            args.output.clone()
        };

        let rannot = if let Some(reference_annotation) = &args.reference_annotation {
            load_sequence_range(reference_annotation, RangeFormat::Bed).unwrap()
        } else {
            Vec::new()
        };
        let qannot = if let Some(query_annotation) = &args.query_annotation {
            load_sequence_range(query_annotation, RangeFormat::Bed).unwrap()
        } else {
            Vec::new()
        };

        let mut dotplot = DotPlot::new(&rseq, &qseq, args.base_per_pixel, dot_color, appearance);
        dotplot.add_annotation(&rannot, &qannot, &annot_color);
        Context {
            basename,
            rseq,
            qseq,
            dotplot,
            dot_color,
            rannot,
            qannot,
            annot_color,
            appearance,
            args: args.clone(),
        }
    }

    fn plot_file(&self, dotplot: &DotPlot) {
        // format name
        let name = if self.args.split_plot {
            format!(
                "{}.{}.{}.png",
                &self.basename,
                dotplot.references()[0].to_path_string(),
                dotplot.queries()[0].to_path_string()
            )
        } else {
            format!("{}.png", &self.basename)
        };
        plot(&name, dotplot).unwrap();
    }

    fn plot_iterm2(&self, dotplot: &DotPlot) {
        let mut file = NamedTempFile::with_suffix(".png").unwrap();
        plot(file.path().to_str().unwrap(), dotplot).unwrap();

        let mut buf = Vec::new();
        file.read_to_end(&mut buf).unwrap();

        let mut img = iterm2img::from_bytes(buf).inline(true).preserve_aspect_ratio(true);
        if let Some(width) = self.args.iterm2_width {
            img = img.width(width as u64);
        }
        if let Some(height) = self.args.iterm2_height {
            img = img.height(height as u64);
        }
        let encoded = img.build();
        println!("{encoded}");
    }

    fn plot(&self, dotplot: &DotPlot) {
        let count = dotplot.get_seed_count();
        if count < self.args.min_count {
            log::info!(
                "skip {:?} x {:?} as seed count {} < {}",
                dotplot.references(),
                dotplot.queries(),
                count,
                self.args.min_count
            );
            return;
        }
        if self.args.iterm2 {
            self.plot_iterm2(dotplot);
        } else {
            self.plot_file(dotplot);
        }
    }

    fn flush(&mut self) {
        let new_dotplot = || {
            let mut dotplot = DotPlot::new(&self.rseq, &self.qseq, self.args.base_per_pixel, self.dot_color, self.appearance);
            dotplot.add_annotation(&self.rannot, &self.qannot, &self.annot_color);
            dotplot
        };
        let dotplot = std::mem::replace(&mut self.dotplot, new_dotplot());
        if self.args.split_plot {
            for dotplot in dotplot.split() {
                self.plot(&dotplot);
            }
        } else {
            self.plot(&dotplot);
        }
    }

    fn add_reference(&mut self, r: &SequenceRange) {
        if self.args.split_plot && self.args.sorted && self.dotplot.has_plane() {
            self.flush();
        }
        if self.rseq.is_empty() {
            self.dotplot.add_reference(r);
        }
    }

    fn add_query(&mut self, q: &SequenceRange) {
        if self.args.split_plot && self.args.sorted && self.dotplot.has_plane() {
            self.flush();
        }
        if self.qseq.is_empty() {
            self.dotplot.add_query(q);
        }
    }

    fn append_seed(&mut self, rname: &str, rpos: usize, is_rev: bool, qname: &str, qpos: usize) {
        self.dotplot.append_seed(rname, rpos, is_rev, qname, qpos);
    }

    fn process_token(&mut self, token: SeedToken) {
        match token {
            SeedToken::NewReference(r) => self.add_reference(&r),
            SeedToken::NewQuery(q) => self.add_query(&q),
            SeedToken::Seed(rname, rpos, is_rev, qname, qpos) => self.append_seed(&rname, rpos, is_rev, &qname, qpos),
        }
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
    let args = Args::parse();

    let default_log_level = if args.quiet { "off" } else { "info" };
    env_logger::init_from_env(env_logger::Env::new().default_filter_or(default_log_level));
    print_args(&std::env::args().collect::<Vec<_>>());

    // check output directory exists
    let dir = Path::new(&args.output).parent().unwrap();
    if dir != Path::new("") && !dir.exists() {
        if args.create_missing_dir {
            log::info!("output directory {dir:?} does not exist. creating it.");
            std::fs::create_dir_all(dir).unwrap();
        } else {
            log::error!("output directory {dir:?} does not exist. abort.");
            return;
        }
    }

    // open input stream
    let stream: Box<dyn Read> = if let Some(query) = &args.query {
        let seed_generator = args.seed_generator.as_ref().unwrap();
        let inputs: [&str; 2] = if args.swap_generator {
            [query, &args.reference]
        } else {
            [&args.reference, query]
        };
        Box::new(SeedGeneratorCommand::new(seed_generator, inputs.as_slice(), args.use_stdout))
    } else {
        Box::new(std::fs::File::open(&args.reference).unwrap())
    };
    let stream = std::io::BufReader::new(stream);

    let min_density = args.min_density.max(0.000001);
    let dot_color = DensityColorMap {
        palette: [RGBColor(255, 0, 64), RGBColor(0, 64, 255)],
        max_density: args.density * args.density / min_density,
        min_density,
    };
    let annot_color = AnnotationColorMap {
        palette: HashMap::new(),
        default: RGBColor(0, 0, 0),
        alpha: 0.05,
        prefix_match: true,
        exact_match: false,
    };

    // create dotplot
    let text_style = TextStyle::from(("sans-serif", 12).into_font()).color(&BLACK);
    let axis_appearance = AxisAppearance {
        axis_thickness: 1,
        large_tick_length: 3,
        small_tick_length: 1,
        label_setback: 8,
        label_style: text_style.clone(),
        fit_in_box: true,
    };
    let appearance = DotPlotAppearance {
        spacer_thickness: 1,
        desired_tick_pitch: 25,

        x_label_area_size: 35,
        x_axis_appearance: axis_appearance.clone(),
        x_seq_name_style: text_style.clone(),
        x_seq_name_setback: 20,

        y_label_area_size: 50,
        y_axis_appearance: axis_appearance.clone(),
        y_seq_name_style: text_style.clone(),
        y_seq_name_setback: 35,
    };
    // parse the stream
    let mut ctx = Context::new(&args, &dot_color, &annot_color, &appearance);
    let parser = SeedParser::new(stream.lines(), args.swap_generator);
    for token in parser {
        ctx.process_token(token);
    }
    log::info!("seed stream reached the end. cleaning...");
    ctx.flush();
}
