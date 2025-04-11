// @file seq.rs
// @author Hajime Suzuki
// @brief sequence (name, range) container and parsers

use anyhow::{Result, anyhow};
use clap::ValueEnum;
use std::fmt;
use std::io::BufRead;
use std::ops::Range;

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum RangeFormat {
    Fasta,
    Bed,  // chr7\t6000000\t6300000
    Text, // chr7:6000000-6300000
    Infer,
}

#[derive(Clone, Default)]
pub struct Seq {
    pub name: String,
    pub range: Range<usize>,
}

impl Seq {
    pub fn to_path_string(&self) -> String {
        format!("{}_{}_{}", self.name, self.range.start, self.range.end)
    }
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

pub fn load_range(file: &str, format: &RangeFormat) -> Result<Vec<Seq>> {
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
            load_range_fasta(file)
        }
    }
}
