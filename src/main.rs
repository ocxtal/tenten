mod command;
mod file;
mod parser;

use crate::command::SeedGeneratorCommand;
use crate::file::CachedFile;
use crate::parser::{SeedParser, SeedToken};
use clap::{Parser, ValueEnum};
use hex_color::HexColor;
use plotters::prelude::*;
use plotters::style::text_anchor::{HPos, Pos, VPos};
use regex::Regex;
use std::collections::HashMap;
use std::io::{BufRead, Read};
use std::path::Path;
use tempfile::NamedTempFile;
use tenten::*;

macro_rules! group_input {
    () => {
        "Seed generator options"
    };
}
macro_rules! group_plot {
    () => {
        "Plot options"
    };
}
macro_rules! group_range {
    () => {
        "Range/label options"
    };
}
macro_rules! group_output {
    () => {
        "Output options"
    };
}

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum OrthogonalNameLabel {
    None,
    Target,
    Query,
    Both,
}

#[derive(Clone, Debug, Parser)]
#[command(version)]
#[command(max_term_width = 160)]
pub struct Args {
    #[clap(
        help = "Target sequence (if query is passed) or seeds in minimap2 --print-seeds format",
        value_name = "TARGET SEQUENCE or PREGENERATED SEEDS"
    )]
    pub target: String,

    #[clap(help = "Query sequence")]
    pub query: Option<String>,

    #[clap(
        help_heading = group_input!(),
        short = 'P',
        long,
        help = "Seed generator command template",
        value_name = "COMMAND TEMPLATE",
        default_value = "minimap2 -t1 --print-seeds {0} {1}"
    )]
    pub seed_generator: Option<String>,

    #[clap(
        help_heading = group_input!(),
        long,
        help = "Secondary seed generator command template for lower triangle (implies -s)",
        value_name = "COMMAND TEMPLATE"
    )]
    pub secondary_seed_generator: Option<String>,

    #[clap(help_heading = group_input!(), short = 'O', long, help = "Use stdout of seed generator, instead of stderr", default_value = "false")]
    pub use_stdout: bool,

    #[clap(help_heading = group_input!(), short = 's', long, help = "Self dotplot. Target sequence is used as query sequence as well", default_value = "false")]
    pub self_dotplot: bool,

    #[clap(help_heading = group_input!(), short = 'x', long, help = "Swap target and query for seed generator", default_value = "false")]
    pub swap_generator: bool,

    #[clap(help_heading = group_plot!(), short = 'b', long, help = "Bases per pixel", value_name = "INT", default_value = "100")]
    pub base_per_pixel: usize,

    #[clap(
        help_heading = group_plot!(),
        short = 'M',
        long,
        help = "Density of seeds (#per 1kbp square) that corresponds to 50% heatmap scale",
        value_name = "FLOAT",
        default_value = "20.0"
    )]
    pub mid_density: f64,

    #[clap(
        help_heading = group_plot!(),
        short = 'm',
        long,
        help = "Density of seeds (#per 1kbp square) that corresponds to 0% heatmap scale",
        value_name = "FLOAT",
        default_value = "0.1"
    )]
    pub min_density: f64,

    #[clap(
        help_heading = group_plot!(),
        short = 'c',
        long,
        help = "Do not output image if #seed is less than this value",
        value_name = "INT",
        default_value = "0"
    )]
    pub min_count: usize,

    #[clap(help_heading = group_plot!(), short = 'X', long, help = "Swap x/y axes of the output plots", default_value = "false")]
    pub swap_plot_axes: bool,

    #[clap(help_heading = group_plot!(), long, help = "Hide scale bars", default_value = "false")]
    pub hide_scale: bool,

    #[clap(
        help_heading = group_range!(),
        short = 't',
        long,
        help = "Plot seeds only in the ranges in the file. Seeds outside the ranges are ignored. Accepts BED or \"chr7:6000000-6300000\" format",
        value_name = "FILE"
    )]
    pub target_range: Option<String>,

    #[clap(help_heading = group_range!(), hide = true, value_name = "FILE", long, value_parser = clap::builder::UnknownArgumentValueParser::suggest_arg("--target-range"))]
    pub reference_range: Option<String>,

    #[clap(
        help_heading = group_range!(),
        short = 'q',
        long,
        help = "Same as above for query",
        value_name = "FILE"
    )]
    pub query_range: Option<String>,

    #[clap(
        help_heading = group_range!(),
        short = 'T',
        long,
        help = "Force treat the target range file in a specific format",
        value_name = "FORMAT",
        default_value = "infer"
    )]
    pub target_range_format: RangeFormat,

    #[clap(help_heading = group_range!(), hide = true, value_name = "FORMAT", long, value_parser = clap::builder::UnknownArgumentValueParser::suggest_arg("--target-range-format"))]
    pub reference_range_format: Option<String>,

    #[clap(
        help_heading = group_range!(),
        short = 'Q',
        long,
        help = "Same as above for query",
        value_name = "FORMAT",
        default_value = "infer"
    )]
    pub query_range_format: RangeFormat,

    #[clap(help_heading = group_range!(), long, help = "Regex to extract virtual name and start coordinate from its sequence name, for use in annotation coloring and plotting", value_name = "REGEX")]
    pub target_extractor: Option<String>,

    #[clap(help_heading = group_range!(), hide = true, value_name = "REGEX", long, value_parser = clap::builder::UnknownArgumentValueParser::suggest_arg("--target-extractor"))]
    pub reference_extractor: Option<String>,

    #[clap(help_heading = group_range!(), long, help = "Same as above for query", value_name = "REGEX")]
    pub query_extractor: Option<String>,

    #[clap(help_heading = group_range!(), long, help = "Annotation file for the target (in BED format)", value_name = "FILE")]
    pub target_annotation: Option<String>,

    #[clap(help_heading = group_range!(), hide = true, value_name = "FILE", long, value_parser = clap::builder::UnknownArgumentValueParser::suggest_arg("--target-annotation"))]
    pub reference_annotation: Option<String>,

    #[clap(help_heading = group_range!(), long, help = "Same as above for query", value_name = "FILE")]
    pub query_annotation: Option<String>,

    #[clap(help_heading = group_range!(), short = 'A', long, help = "Annotation color configuration", value_name = "FILE")]
    pub annotation_color: Option<String>,

    #[clap(help_heading = group_range!(), long, help = "Place sequence name labels orthogonal to the axes")]
    pub orthogonal_name_label: OrthogonalNameLabel,

    #[clap(
        help_heading = group_output!(),
        short = 'o',
        long,
        help = "Output filename (prefix if split plot)",
        default_value = "out.png",
        value_name = "FILE"
    )]
    pub output: String,

    #[clap(help_heading = group_output!(), short = 'F', long, help = "Create directory if it doesn't exist", default_value = "false")]
    pub create_missing_dir: bool,

    #[clap(help_heading = group_output!(), short = 'p', long, help = "Create plot for each target/query pair", default_value = "false")]
    pub split_plot: bool,

    #[clap(
        help_heading = group_output!(),
        short = 'S',
        long,
        help = "Assume the seed generator output is sorted by query and reference names (reduces memory usage when --split-plot)",
        default_value = "false"
    )]
    pub sorted: bool,

    #[clap(
        help_heading = group_output!(),
        short = 'I',
        long,
        help = "Print plot to terminal (encoded to iTerm2 image format)",
        default_value = "false"
    )]
    pub iterm2: bool,

    #[clap(help_heading = group_output!(), short = 'W', long, help = "Width in characters when printing plot to terminal", value_name = "INT")]
    pub iterm2_width: Option<usize>,

    #[clap(help_heading = group_output!(), short = 'H', long, help = "Height in characters when printing plot to terminal", value_name = "INT")]
    pub iterm2_height: Option<usize>,

    #[clap(long, help = "Suppress logs", default_value = "false")]
    pub quiet: bool,
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
                    seq.virtual_start = Some(start);
                }
            } else if let Some(offset) = cap.name("offset") {
                log::debug!("offset found: {:?}", offset.as_str());
                if let Ok(offset) = offset.as_str().parse::<isize>() {
                    seq.virtual_start = Some(offset);
                }
            }

            if let Some(name) = cap.name("name") {
                log::debug!("name found: {:?}", name.as_str());
                seq.virtual_name = Some(name.as_str().to_string());
            }
            let name = seq.virtual_name();
            let range = seq.virtual_range();
            log::debug!("sequence patched: {}:{}-{}", name, range.start, range.end);
        }
    }
}

fn load_annotation_palette(file: &str) -> HashMap<String, RGBColor> {
    let file = std::io::BufReader::new(std::fs::File::open(file).unwrap());
    let mut palette = HashMap::new();

    for line in file.lines() {
        let line = line.unwrap();
        if line.starts_with('#') || line.is_empty() {
            continue;
        }

        let mut cols = line.split('\t');
        let name = cols.next().unwrap();
        let color = HexColor::parse(cols.next().unwrap()).unwrap();
        palette.insert(name.to_string(), RGBColor(color.r, color.g, color.b));
    }
    log::debug!("annotation palette loaded: {palette:?}");
    palette
}

struct Context<'a> {
    suffix: String,
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
        target: &str,
        query: Option<&str>,
        dot_color: &'a DensityColorMap,
        annot_color: &'a AnnotationColorMap,
        appearance: &'a DotPlotAppearance<'a>,
    ) -> Self {
        let (rseq, qseq) = if let Some(query) = query {
            let rseq = load_sequence_range(target, RangeFormat::Fasta).unwrap();
            let qseq = load_sequence_range(query, RangeFormat::Fasta).unwrap();
            (rseq, qseq)
        } else {
            (Vec::new(), Vec::new())
        };

        let mut rseq = if let Some(target_range) = &args.target_range {
            load_sequence_range(target_range, args.target_range_format).unwrap()
        } else {
            rseq
        };
        let mut qseq = if let Some(query_range) = &args.query_range {
            load_sequence_range(query_range, args.query_range_format).unwrap()
        } else {
            qseq
        };

        if let Some(ref re) = args.target_extractor {
            let extractor = LabelExtractor::new(re);
            for seq in &mut rseq {
                extractor.patch_sequence(seq);
            }
        }
        if let Some(ref re) = args.query_extractor {
            let extractor = LabelExtractor::new(re);
            for seq in &mut qseq {
                extractor.patch_sequence(seq);
            }
        }

        let (rseq, qseq) = if args.swap_generator { (qseq, rseq) } else { (rseq, qseq) };

        log::debug!("target ranges: {rseq:?}");
        log::debug!("query ranges: {qseq:?}");

        let (suffix, basename) = if let Some(basename) = args.output.strip_suffix(".png") {
            (".png".to_string(), basename.to_string())
        } else if let Some(basename) = args.output.strip_suffix(".svg") {
            (".svg".to_string(), basename.to_string())
        } else {
            (".png".to_string(), args.output.clone())
        };

        let rannot = if let Some(target_annotation) = &args.target_annotation {
            load_sequence_range(target_annotation, RangeFormat::Bed).unwrap()
        } else {
            Vec::new()
        };
        let qannot = if let Some(query_annotation) = &args.query_annotation {
            load_sequence_range(query_annotation, RangeFormat::Bed).unwrap()
        } else {
            Vec::new()
        };
        log::debug!("target annotations: {rannot:?}");
        log::debug!("query annotations: {qannot:?}");

        let mut dotplot = DotPlot::new(&rseq, &qseq, args.base_per_pixel, dot_color, appearance);
        dotplot.add_annotation(&rannot, &qannot, annot_color);
        Context {
            suffix,
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
                "{}.{}.{}{}",
                &self.basename,
                dotplot.targets()[0].to_path_string(),
                dotplot.queries()[0].to_path_string(),
                &self.suffix
            )
        } else {
            format!("{}{}", &self.basename, &self.suffix)
        };
        plot(&name, dotplot, self.args.hide_scale).unwrap();
    }

    fn plot_iterm2(&self, dotplot: &DotPlot) {
        let mut file = NamedTempFile::with_suffix(&self.suffix).unwrap();
        plot(file.path().to_str().unwrap(), dotplot, self.args.hide_scale).unwrap();

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
                dotplot.targets(),
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
            dotplot.add_annotation(&self.rannot, &self.qannot, self.annot_color);
            dotplot
        };
        let dotplot = std::mem::replace(&mut self.dotplot, new_dotplot());
        let dotplot = if self.args.swap_plot_axes { dotplot.swap_axes() } else { dotplot };

        if self.args.split_plot {
            for dotplot in dotplot.split() {
                self.plot(&dotplot);
            }
        } else {
            self.plot(&dotplot);
        }
    }

    fn add_target(&mut self, r: &SequenceRange) {
        if self.args.split_plot && self.args.sorted && self.dotplot.has_plane() {
            self.flush();
        }
        if self.rseq.is_empty() {
            self.dotplot.add_target(r);
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
            SeedToken::NewTarget(r) => self.add_target(&r),
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

    let target = CachedFile::new(&args.target);
    let query = if args.self_dotplot {
        Some(target.alias())
    } else {
        args.query.as_deref().map(CachedFile::new)
    };

    let stream: Box<dyn Read> = if let Some(query) = &query {
        let seed_generator = args.seed_generator.as_ref().unwrap();
        let inputs: [&str; 2] = if args.swap_generator {
            [query.name(), target.name()]
        } else {
            [target.name(), query.name()]
        };
        Box::new(SeedGeneratorCommand::new(seed_generator, inputs.as_slice(), args.use_stdout))
    } else {
        Box::new(std::fs::File::open(&args.target).unwrap())
    };
    let stream = std::io::BufReader::new(stream);

    let min_density = args.min_density.max(0.000001);
    let dot_color = DensityColorMap {
        palette: [RGBColor(255, 0, 64), RGBColor(0, 64, 255)],
        max_density: args.mid_density * args.mid_density / min_density,
        min_density,
    };
    let annot_color = AnnotationColorMap {
        palette: args
            .annotation_color
            .as_deref()
            .map(load_annotation_palette)
            .unwrap_or_else(HashMap::new),
        default: RGBColor(0, 0, 0),
        alpha: 0.15,
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
    let mut appearance = DotPlotAppearance {
        spacer_thickness: 1,
        desired_tick_pitch: 25,

        x_label_area_size: 50,
        x_axis_appearance: axis_appearance.clone(),
        x_seq_name_style: text_style.clone().pos(Pos::new(HPos::Center, VPos::Top)),
        x_seq_name_setback: 30,

        y_label_area_size: 70,
        y_axis_appearance: axis_appearance.clone(),
        y_seq_name_style: text_style
            .clone()
            .pos(Pos::new(HPos::Center, VPos::Bottom))
            .transform(FontTransform::Rotate270),
        y_seq_name_setback: 50,
    };

    if matches!(args.orthogonal_name_label, OrthogonalNameLabel::Target | OrthogonalNameLabel::Both) {
        appearance.x_label_area_size = 235;
        appearance.x_seq_name_style = text_style
            .clone()
            .pos(Pos::new(HPos::Right, VPos::Center))
            .transform(FontTransform::Rotate270);
        appearance.x_seq_name_setback = 20;
    }
    if matches!(args.orthogonal_name_label, OrthogonalNameLabel::Query | OrthogonalNameLabel::Both) {
        appearance.y_label_area_size = 250;
        appearance.y_seq_name_style = text_style.clone().pos(Pos::new(HPos::Right, VPos::Center));
        appearance.y_seq_name_setback = 50;
    }

    // parse the stream
    let mut ctx = Context::new(
        &args,
        target.name(),
        query.as_ref().map(|x| x.name()),
        &dot_color,
        &annot_color,
        &appearance,
    );
    let parser = SeedParser::new(stream.lines(), args.swap_generator);
    for token in parser {
        ctx.process_token(token);
    }
    log::info!("seed stream reached the end. cleaning...");
    ctx.flush();
}
