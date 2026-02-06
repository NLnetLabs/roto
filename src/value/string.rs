use std::{borrow::Borrow, sync::Arc};

use crate::List;

/// Inner data type containing the string
#[derive(Clone, Default, PartialEq, Eq)]
struct StringData(Arc<str>);

/// Roto's builti-in string type
#[derive(Clone, Default, PartialEq, Eq)]
#[repr(transparent)]
pub struct String(StringData);

impl String {
    /// Create a new [`String`].
    pub fn new(x: impl Into<Arc<str>>) -> Self {
        Self(StringData(x.into()))
    }

    /// Create a [`String`] from a [`List<char>`].
    pub fn from_chars(list: List<char>) -> Self {
        let mut out = std::string::String::new();
        for item in list.to_vec() {
            out.push(item);
        }
        out.into()
    }

    /// Returns whether `needle` is a substring of `self`.
    pub fn contains(&self, needle: &str) -> bool {
        self.0.0.contains(needle)
    }

    /// Returns whether `self` starts with the substring `prefix`.
    pub fn starts_with(&self, prefix: &str) -> bool {
        self.0.0.starts_with(prefix)
    }

    /// Returns whether `self` ends with the substring `suffix`.
    pub fn ends_with(&self, suffix: &str) -> bool {
        self.0.0.ends_with(suffix)
    }

    /// Convert this string to lowercase.
    pub fn to_lowercase(&self) -> Self {
        self.0.0.to_lowercase().into()
    }

    /// Convert this string to lowercase.
    pub fn to_uppercase(&self) -> Self {
        self.0.0.to_uppercase().into()
    }

    /// Create a new string by repeating this string `n` times.
    pub fn repeat(&self, n: usize) -> Self {
        self.0.0.repeat(n).into()
    }

    /// Create a list of strings by splitting this string by the `separator`.
    pub fn split(&self, separator: &str) -> List<String> {
        self.0.0.split(&separator).map(Into::into).collect()
    }

    /// Replace each substring `from` with `to`.
    pub fn replace(self, from: &str, to: &str) -> Self {
        self.0.0.replace(from, to).into()
    }

    /// Get a view of this string indexed by bytes.
    pub fn bytes(self) -> StringBytes {
        StringBytes(self.0)
    }

    /// Get a view of this string indexed by chars.
    pub fn chars(self) -> StringChars {
        StringChars(self.0)
    }

    /// Get a view of this string indexed by lines.
    pub fn lines(self) -> StringLines {
        StringLines(self.0)
    }
}

impl<T: Into<Arc<str>>> From<T> for String {
    fn from(value: T) -> Self {
        String(StringData(value.into()))
    }
}

impl From<String> for std::string::String {
    fn from(value: String) -> std::string::String {
        std::string::String::from(&*value.0.0)
    }
}

impl AsRef<str> for String {
    fn as_ref(&self) -> &str {
        &self.0.0
    }
}

impl Borrow<str> for String {
    fn borrow(&self) -> &str {
        &self.0.0
    }
}

impl std::ops::Deref for String {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0.0
    }
}

impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.0.fmt(f)
    }
}

impl std::fmt::Debug for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.0.fmt(f)
    }
}

/// A view into a string indexed by bytes.
#[derive(Clone)]
#[repr(transparent)]
pub struct StringBytes(StringData);

impl StringBytes {
    /// Get the length of the string in bytes.
    pub fn len(&self) -> usize {
        self.0.0.len()
    }

    /// Get the character at byte offset `idx`.
    pub fn get(&self, idx: usize) -> Option<char> {
        self.0.0.get(idx..).and_then(|s| s.chars().next())
    }

    /// Slice this string based on byte indices.
    ///
    /// This method returns `None` if either `i` or `j` is out of bounds or if
    /// `i` is greater than `j`.
    pub fn slice(&self, i: usize, j: usize) -> Option<String> {
        self.0.0.get(i..j).map(Into::into)
    }

    /// Returns the list of bytes of this string
    pub fn list(&self) -> List<u8> {
        // TODO: This could be optimized
        self.0.0.as_bytes().iter().copied().collect()
    }
}

/// A view into a string indexed by code points.
#[derive(Clone)]
#[repr(transparent)]
pub struct StringChars(StringData);

impl StringChars {
    /// Get the number of characters in a string.
    pub fn len(&self) -> usize {
        self.0.0.chars().count()
    }

    /// Get the nth character of this string.
    pub fn get(&self, idx: usize) -> Option<char> {
        self.0.0.chars().nth(idx)
    }

    /// Slice this string based on the character indices.
    ///
    /// This method returns `None` if either `i` or `j` is out of bounds or if
    /// `i` is greater than `j`.
    pub fn slice(&self, i: usize, j: usize) -> Option<String> {
        // If j is less than i, we return None.
        let len = j.checked_sub(i)?;

        // Create an iterator for character indices. We have to chain it
        // with the length of the string because that index won't be
        // returned by the char_indices iterator.
        let mut indices = self
            .0
            .0
            .char_indices()
            .map(|(byte, _)| byte)
            .chain(std::iter::once(self.0.0.len()));

        let byte_i = indices.nth(i)?;

        // We need to determine how many characters we have to advance
        // the iterator, which means subtracting with 1.
        if let Some(idx) = len.checked_sub(1) {
            let byte_j = indices.nth(idx)?;
            Some(self.0.0[byte_i..byte_j].into())
        } else {
            Some("".into())
        }
    }

    /// Get a list of characters that this string consists of.
    pub fn list(&self) -> List<char> {
        let list = List::new();

        for char in self.0.0.chars() {
            list.push(char);
        }

        list
    }
}

/// A view into a string indexed by lines.
#[derive(Clone)]
#[repr(transparent)]
pub struct StringLines(StringData);

impl StringLines {
    /// Get the number of lines in this string.
    pub fn len(&self) -> usize {
        self.0.0.lines().count()
    }

    /// Get the nth line in this string.
    pub fn get(&self, idx: usize) -> Option<char> {
        self.0.0.get(idx..).and_then(|s| s.chars().next())
    }

    /// Slice this string by lines.
    ///
    /// This method returns `None` if either `i` or `j` is out of bounds or if
    /// `i` is greater than `j`.
    pub fn slice(&self, i: usize, j: usize) -> Option<String> {
        // If j is less than i, we return None.
        j.checked_sub(i)?;

        let mut iter = self.0.0.lines();

        // This is essentially a manual `Iterator::skip` implementation, except
        // that we return `None` if `i` is out of bounds.
        for _ in 0..i {
            iter.next()?;
        }

        // Same as above but for `Iterator::take`
        let mut s = std::string::String::new();
        for _ in i..j {
            s.push_str(iter.next()?);
        }

        Some(s.into())
    }

    /// Get a list of lines
    pub fn list(&self) -> List<String> {
        // TODO: This could be optimized
        self.0.0.lines().map(Into::into).collect()
    }
}
