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
    NewTarget(SequenceRange),
    NewQuery(SequenceRange),
    Seed(String, usize, bool, String, usize),
    ChainAnchor(String, usize, bool, String, usize),
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
            Some(SeedToken::NewTarget(SequenceRange {
                name,
                range: 0..len,
                annotation: None,
                virtual_name: None,
                virtual_start: None,
            }))
        } else {
            Some(SeedToken::NewQuery(SequenceRange {
                name,
                range: 0..len,
                annotation: None,
                virtual_name: None,
                virtual_start: None,
            }))
        }
    }

    fn parse_seed_mm2(&self, line: &str) -> Option<SeedToken> {
        // SD      chr1_mat     159     +       31480   15      0
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        assert!(cols.len() == 7, "{:?}", line);
        assert!(cols[3] == "-" || cols[3] == "+");

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

    fn parse_chain_mm2(&self, line: &str) -> Option<SeedToken> {
        // CN      0       chr1_mat     159     +       31480   15      0
        let cols = line.trim().split('\t').collect::<Vec<_>>();
        assert!(cols.len() == 8, "{:?}", line);
        assert!(cols[4] == "-" || cols[4] == "+");

        let rname = cols[2].to_string();
        let rpos = cols[3].parse::<usize>().unwrap();
        let is_rev = cols[4] == "-";
        let qname = self.query_cache.clone().unwrap();
        let qpos = cols[5].parse::<usize>().unwrap();

        let (rname, rpos, qname, qpos) = if self.swap {
            (qname, qpos, rname, rpos)
        } else {
            (rname, rpos, qname, qpos)
        };

        Some(SeedToken::ChainAnchor(rname, rpos, is_rev, qname, qpos))
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
            Some(SeedToken::NewTarget(s))
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
            } else if line.starts_with("CN") {
                return self.parse_chain_mm2(line);
            } else if line.starts_with("QM") || line.starts_with("QT") || line.starts_with("RS") {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn ok(line: &str) -> std::io::Result<String> {
        Ok(line.to_string())
    }

    #[test]
    fn parses_chain_anchor() {
        let lines = vec![ok("QR\tquery\t0\t1000"), ok("CN\t0\tchr1\t100\t+\t200\t15\t0")];
        let mut parser = SeedParser::new(lines.into_iter(), false);
        assert!(matches!(parser.next(), Some(SeedToken::NewQuery(_))));

        match parser.next() {
            Some(SeedToken::ChainAnchor(rname, rpos, is_rev, qname, qpos)) => {
                assert_eq!(rname, "chr1");
                assert_eq!(rpos, 100);
                assert!(!is_rev);
                assert_eq!(qname, "query");
                assert_eq!(qpos, 200);
            }
            token => panic!("unexpected token: {token:?}"),
        }
    }

    #[test]
    fn parses_reverse_chain_anchor() {
        let lines = vec![ok("QR\tquery\t0\t1000"), ok("CN\t0\tchr1\t100\t-\t200\t15\t0")];
        let mut parser = SeedParser::new(lines.into_iter(), false);
        assert!(matches!(parser.next(), Some(SeedToken::NewQuery(_))));

        match parser.next() {
            Some(SeedToken::ChainAnchor(_, _, is_rev, _, _)) => assert!(is_rev),
            token => panic!("unexpected token: {token:?}"),
        }
    }

    #[test]
    fn swaps_chain_anchor() {
        let lines = vec![ok("QR\tquery\t0\t1000"), ok("CN\t0\tchr1\t100\t+\t200\t15\t0")];
        let mut parser = SeedParser::new(lines.into_iter(), true);
        assert!(matches!(parser.next(), Some(SeedToken::NewTarget(_))));

        match parser.next() {
            Some(SeedToken::ChainAnchor(rname, rpos, is_rev, qname, qpos)) => {
                assert_eq!(rname, "query");
                assert_eq!(rpos, 200);
                assert!(!is_rev);
                assert_eq!(qname, "chr1");
                assert_eq!(qpos, 100);
            }
            token => panic!("unexpected token: {token:?}"),
        }
    }
}
