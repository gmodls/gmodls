use std::cmp::Ordering;

/// Represents a region within a source file.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    /// Returns a new [`Span`] with the given `start` and `end` positions.
    pub fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Pos {
        self.start
    }

    pub fn end(&self) -> Pos {
        self.end
    }
}

/// Represents a position within a source file.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Pos {
    offset: usize,

    row: usize,
    col: usize,
}

impl Pos {
    /// Returns a new [`Pos`] with the given `offset`, `row`, and `col`.
    pub fn new(offset: usize, row: usize, col: usize) -> Self {
        Self { offset, row, col }
    }
}

impl PartialOrd<Self> for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}
