use std::ops::Index;
use std::path::Path;
use std::{fs, io};

#[derive(Debug, thiserror::Error)]
pub enum SourceError {
    #[error("An IO error has occurred: {from}")]
    IO {
        #[from]
        from: io::Error,
    },
}

pub type SourceResult<T> = Result<T, SourceError>;

#[derive(Debug, PartialEq, Eq)]
pub struct Source {
    contents: Vec<u8>,
}

impl Source {
    /// Reads the data in the file specified into a new [`Source`].
    pub fn from_file<P: AsRef<Path>>(path: P) -> SourceResult<Self> {
        Ok(Self {
            contents: fs::read(path)?,
        })
    }

    /// Returns the length of the contents.
    #[inline]
    pub fn len(&self) -> usize {
        self.contents.len()
    }
}

impl From<&[u8]> for Source {
    fn from(value: &[u8]) -> Self {
        Self {
            contents: Vec::from(value),
        }
    }
}

impl Index<usize> for Source {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.contents[index]
    }
}
