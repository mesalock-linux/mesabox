use nom::*;

use std::iter::Enumerate;
use std::slice::Iter as SliceIter;
use std::ops::{Index, Range, RangeTo, RangeFrom, RangeFull};

use super::Token;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenIter<'a, 'b: 'a> {
    tokens: &'a [Token<'b>],
}

impl<'a, 'b> TokenIter<'a, 'b> {
    pub fn new(input: &'a [Token<'b>]) -> Self {
        Self {
            tokens: input,
        }
    }

    pub fn get(&self, idx: usize) -> &'a Token<'b> {
        &self.tokens[idx]
    }
}

impl<'a, 'b> InputLength for TokenIter<'a, 'b> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a, 'b> AtEof for TokenIter<'a, 'b> {
    fn at_eof(&self) -> bool {
        true
    }
}

impl<'a, 'b> InputTake for TokenIter<'a, 'b> {
    fn take(&self, count: usize) -> Self {
        self.slice(..count)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (begin, end) = self.tokens.split_at(count);
        (TokenIter { tokens: end }, TokenIter { tokens: begin })
    }
}

impl<'a> InputLength for Token<'a> {
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a, 'b> Slice<Range<usize>> for TokenIter<'a, 'b> {
    fn slice(&self, range: Range<usize>) -> Self {
        TokenIter { tokens: self.tokens.slice(range) }
    }
}

impl<'a, 'b> Slice<RangeTo<usize>> for TokenIter<'a, 'b> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a, 'b> Slice<RangeFrom<usize>> for TokenIter<'a, 'b> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.tokens.len())
    }
}

impl<'a, 'b> Slice<RangeFull> for TokenIter<'a, 'b> {
    fn slice(&self, _: RangeFull) -> Self {
        self.clone()
    }
}

impl<'a, 'b> InputIter for TokenIter<'a, 'b> {
    type Item = &'a Token<'b>;
    type RawItem = Self::Item;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = SliceIter<'a, Token<'b>>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::RawItem) -> bool,
    {
        self.tokens.iter().position(|b| predicate(b))
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tokens.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

impl<'a, 'b> Index<usize> for TokenIter<'a, 'b> {
    type Output = Token<'b>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}
