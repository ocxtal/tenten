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
pub struct SequenceRange {
    pub name: String,
    pub range: Range<usize>,
    pub annotation: Option<String>,
    pub virtual_name: Option<String>,
    pub virtual_start: isize,
}

impl SequenceRange {
    pub fn to_path_string(&self) -> String {
        let name = self.virtual_name();
        let range = self.virtual_range();
        format!("{}_{}_{}", name, range.start, range.end)
    }

    pub fn subrange(&self, other: &SequenceRange) -> Option<SequenceRange> {
        if self.name != other.name {
            return None;
        }
        let start = self.range.start.max(other.range.start);
        let end = self.range.end.min(other.range.end);
        if start >= end {
            return None;
        }
        Some(SequenceRange {
            name: self.name.clone(),
            range: start..end,
            annotation: None,
            virtual_name: None,
            virtual_start: 0,
        })
    }

    pub fn virtual_name(&self) -> &str {
        if let Some(ref name) = self.virtual_name { name } else { &self.name }
    }

    pub fn virtual_range(&self) -> Range<isize> {
        let start = self.range.start as isize + self.virtual_start;
        let end = self.range.end as isize + self.virtual_start;
        start..end
    }
}

impl fmt::Display for SequenceRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.name, self.range.start, self.range.end)
    }
}

impl fmt::Debug for SequenceRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}-{}", self.name, self.range.start, self.range.end)
    }
}

fn load_sequence_range_fasta(file: &str) -> Result<Vec<SequenceRange>> {
    let file = std::fs::File::open(file)?;
    let file = std::io::BufReader::new(file);

    let mut v = Vec::new();
    let (mut name, mut len) = (None, 0);
    for line in file.lines() {
        let line = line?;
        if line.starts_with('>') {
            if let Some(name) = std::mem::take(&mut name) {
                v.push(SequenceRange {
                    name,
                    range: 0..len,
                    annotation: None,
                    virtual_name: None,
                    virtual_start: 0,
                });
            }

            let cols = line.split_ascii_whitespace().collect::<Vec<_>>();
            assert!(!cols.is_empty() && !cols[0].is_empty());
            (name, len) = (Some(cols[0][1..].to_string()), 0);
        } else {
            len += line.trim().len();
        }
    }
    if let Some(name) = std::mem::take(&mut name) {
        v.push(SequenceRange {
            name,
            range: 0..len,
            annotation: None,
            virtual_name: None,
            virtual_start: 0,
        });
    }

    Ok(v)
}

fn load_sequence_range_bed(file: &str) -> Result<Vec<SequenceRange>> {
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
        let annotation = cols.get(3).map(|s| s.to_string());
        v.push(SequenceRange {
            name,
            range: start..end,
            annotation,
            virtual_name: None,
            virtual_start: 0,
        })
    }
    Ok(v)
}

fn load_sequence_range_text(file: &str) -> Result<Vec<SequenceRange>> {
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
        v.push(SequenceRange {
            name,
            range: start..end,
            annotation: None,
            virtual_name: None,
            virtual_start: 0,
        })
    }
    Ok(v)
}

pub fn load_sequence_range(file: &str, format: RangeFormat) -> Result<Vec<SequenceRange>> {
    match format {
        RangeFormat::Fasta => load_sequence_range_fasta(file),
        RangeFormat::Bed => load_sequence_range_bed(file),
        RangeFormat::Text => load_sequence_range_text(file),
        RangeFormat::Infer => {
            if let Ok(r) = load_sequence_range_text(file) {
                return Ok(r);
            } else if let Ok(r) = load_sequence_range_bed(file) {
                return Ok(r);
            }
            load_sequence_range_fasta(file)
        }
    }
}
