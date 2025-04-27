use std::io::{Seek, SeekFrom, Write};
use tempfile::NamedTempFile;

pub struct CachedFile<'a> {
    pub original: &'a str,
    pub cached: Option<NamedTempFile>,
}

impl<'a> CachedFile<'a> {
    pub fn new(filename: &'a str) -> CachedFile<'a> {
        let mut file = std::fs::File::open(filename).unwrap();
        if file.seek(SeekFrom::Current(0)).is_ok() {
            CachedFile {
                original: filename,
                cached: None,
            }
        } else {
            let mut cached = NamedTempFile::new().unwrap();
            std::io::copy(&mut file, &mut cached).unwrap();
            cached.flush().unwrap();
            CachedFile {
                original: filename,
                cached: Some(cached),
            }
        }
    }

    pub fn alias(&'a self) -> CachedFile<'a> {
        CachedFile {
            original: self.name(),
            cached: None,
        }
    }

    pub fn name(&'a self) -> &'a str {
        if let Some(ref file) = self.cached {
            file.path().to_str().unwrap()
        } else {
            &self.original
        }
    }
}
