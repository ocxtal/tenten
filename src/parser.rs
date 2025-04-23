// @file parser.rs
// @author Hajime Suzuki
// @brief minimap2 seed dump (--print-seeds) parser

use tenten::SequenceRange;

pub struct SeedParser<T>
where
    T: Iterator<Item = std::io::Result<String>>,
{
    it: T,
    swap: bool,
    query_cache: Option<String>,
}

#[derive(Debug)]
pub enum SeedToken {
    NewReference(SequenceRange),
    NewQuery(SequenceRange),
    Seed(String, usize, bool, String, usize),
}

impl<T> SeedParser<T>
where
    T: Iterator<Item = std::io::Result<String>>,
{
    pub fn new(it: T, swap: bool) -> SeedParser<T> {
        SeedParser {
            it,
            swap,
            query_cache: None,
        }
    }

    fn parse_query_mm2(&mut self, line: &str) -> Option<SeedToken> {
        // QR      ptg000001l     0       4865381
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        let name = cols[1].to_string();
        let len = cols[3].parse::<usize>().unwrap();

        self.query_cache = Some(name.clone());
        if self.swap {
            Some(SeedToken::NewReference(SequenceRange {
                name,
                range: 0..len,
                annotation: None,
                name_in_plot: None,
                offset_to_coord_in_plot: 0,
            }))
        } else {
            Some(SeedToken::NewQuery(SequenceRange {
                name,
                range: 0..len,
                annotation: None,
                name_in_plot: None,
                offset_to_coord_in_plot: 0,
            }))
        }
    }

    fn parse_seed_mm2(&self, line: &str) -> Option<SeedToken> {
        // SD      chr1_mat     159     +       31480   15      0
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        let rname = cols[1].to_string();
        let rpos = cols[2].parse::<usize>().unwrap();
        let is_rev = cols[3] == "-";
        let qname = self.query_cache.clone().unwrap();
        let qpos = cols[4].parse::<usize>().unwrap();

        let (rname, rpos, qname, qpos) = if self.swap {
            (qname, qpos, rname, rpos)
        } else {
            (rname, rpos, qname, qpos)
        };

        Some(SeedToken::Seed(rname, rpos, is_rev, qname, qpos))
    }

    fn parse_seq(is_query: bool, line: &str) -> Option<SeedToken> {
        let mut s = SequenceRange::default();
        for (i, col) in line.split('\t').enumerate() {
            match i {
                0 => s.name = col.to_string(),
                1 => s.range = 0..col.parse::<usize>().unwrap(),
                2.. => return None,
            }
        }
        if is_query {
            Some(SeedToken::NewQuery(s))
        } else {
            Some(SeedToken::NewReference(s))
        }
    }

    fn parse_seed(&self, line: &str) -> Option<SeedToken> {
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

        Some(SeedToken::Seed(rname, rpos, is_rev, qname, qpos))
    }
}

impl<T> Iterator for SeedParser<T>
where
    T: Iterator<Item = std::io::Result<String>>,
{
    type Item = SeedToken;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(line) = self.it.next() {
            let line = line.ok()?;
            let line = line.trim();
            if line.starts_with("[") {
                // minimap2 log
                continue;
            } else if line.starts_with("@") {
                // sam header
                return None;
            } else if line.starts_with("QR") {
                return self.parse_query_mm2(line);
            } else if line.starts_with("SD") {
                return self.parse_seed_mm2(line);
            } else if line.starts_with("CN") || line.starts_with("QM") || line.starts_with("QT") || line.starts_with("RS") {
                // ignore
            } else if let Some(body) = line.strip_prefix("#ref\t") {
                return Self::parse_seq(self.swap, body);
            } else if let Some(body) = line.strip_prefix("#query\t") {
                return Self::parse_seq(!self.swap, body);
            } else {
                return self.parse_seed(line);
            }
        }
        None
    }
}
