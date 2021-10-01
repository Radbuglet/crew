use core::str::next_code_point;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::PathBuf;
use std::str::{from_utf8, Utf8Error};
use std::sync::Arc;

// === Logical source === //

#[derive(Clone)]
pub struct SourceFile {
    arc: Arc<SourceFileInner>,
}

struct SourceFileInner {
    path: PathBuf,
    bytes: Vec<u8>,
}

impl SourceFile {
    pub fn from_file(path: PathBuf) -> io::Result<Self> {
        let bytes = fs::read(&path)?;
        Ok(Self::from_bytes(path, bytes))
    }

    pub fn from_bytes(path: PathBuf, bytes: Vec<u8>) -> Self {
        Self {
            arc: Arc::new(SourceFileInner { path, bytes }),
        }
    }

    pub fn head(&self) -> FileLocRef {
        FileLocRef {
            file: self,
            raw: FileLocRaw::HEAD,
        }
    }
}

impl AsFileReader for SourceFile {
    fn reader(&self) -> FileReader {
        FileReader::new(self, &self.arc.bytes, FileLocRaw::HEAD)
    }
}

impl Hash for SourceFile {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(Arc::as_ptr(&self.arc) as usize)
    }
}

impl Eq for SourceFile {}
impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.arc, &other.arc)
    }
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_struct("SourceFile")
            .field("path", &self.arc.path)
            .finish_non_exhaustive()
    }
}

pub type Span = AnySpan<SourceFile>;
pub type SpanRef<'a> = AnySpan<&'a SourceFile>;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct AnySpan<F> {
    file: F,
    start: FileLocRaw,
    end: FileLocRaw,
}

impl Span {
    pub fn new<FA, FB>(a: &AnyFileLoc<FA>, b: &AnyFileLoc<FB>) -> Self
    where
        FA: Borrow<SourceFile>,
        FB: Borrow<SourceFile>,
    {
        SpanRef::new(a, b).as_owned()
    }
}

impl<'a> SpanRef<'a> {
    pub fn new<FA, FB>(a: &'a AnyFileLoc<FA>, b: &AnyFileLoc<FB>) -> Self
    where
        FA: Borrow<SourceFile>,
        FB: Borrow<SourceFile>,
    {
        assert_eq!(
            a.file(),
            b.file(),
            "Cannot construct a Span spanning two different files!"
        );

        let mut locs = [a.raw, b.raw];
        locs.sort();

        Self {
            file: a.file(),
            start: locs[0],
            end: locs[1],
        }
    }
}

impl<F: Borrow<SourceFile>> AnySpan<F> {
    pub fn file(&self) -> &SourceFile {
        self.file.borrow()
    }

    pub fn bytes(&self) -> &[u8] {
        &self.file().arc.bytes[self.start.index..=self.end.index]
    }

    pub fn try_as_str(&self) -> Result<&str, Utf8Error> {
        from_utf8(self.bytes())
    }

    pub fn as_str(&self) -> &str {
        self.try_as_str().unwrap()
    }

    pub fn start(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            raw: self.start,
        }
    }

    pub fn end(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            raw: self.end,
        }
    }

    fn set_pair_unchecked(&mut self, a: FileLocRaw, b: FileLocRaw) {
        let mut locs = [a, b];
        locs.sort();
        self.start = locs[0];
        self.end = locs[1];
    }

    pub fn set_left<FA: Borrow<SourceFile>>(&mut self, loc: &AnyFileLoc<FA>) {
        assert_eq!(
            loc.file(),
            self.file(),
            "Cannot set span start from a different file!"
        );

        self.set_pair_unchecked(loc.raw, self.end);
    }

    pub fn set_right<FA: Borrow<SourceFile>>(&mut self, loc: &AnyFileLoc<FA>) {
        assert_eq!(
            loc.file(),
            self.file(),
            "Cannot set span end from a different file!"
        );

        self.set_pair_unchecked(self.start, loc.raw);
    }

    pub fn as_ref(&self) -> SpanRef {
        SpanRef {
            file: self.file(),
            start: self.start,
            end: self.end,
        }
    }

    pub fn as_owned(&self) -> Span {
        Span {
            file: self.file().clone(),
            start: self.start,
            end: self.end,
        }
    }
}

impl<F: Borrow<SourceFile>> AsFileReader for AnySpan<F> {
    fn reader(&self) -> FileReader {
        FileReader::new(self.file(), self.bytes(), self.start)
    }
}

pub type FileLoc = AnyFileLoc<SourceFile>;
pub type FileLocRef<'a> = AnyFileLoc<&'a SourceFile>;

#[derive(Debug, Copy, Clone, Hash)]
pub struct AnyFileLoc<F> {
    file: F,
    raw: FileLocRaw,
}

impl<F> AnyFileLoc<F> {
    pub fn pos(&self) -> FilePos {
        self.raw.pos
    }

    pub fn line(&self) -> usize {
        self.raw.line()
    }

    pub fn col(&self) -> usize {
        self.raw.col()
    }
}

impl<F: Borrow<SourceFile>> AnyFileLoc<F> {
    pub fn file(&self) -> &SourceFile {
        self.file.borrow()
    }

    pub fn as_span(&self) -> SpanRef {
        SpanRef::new(self, self)
    }

    pub fn as_ref(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            raw: self.raw,
        }
    }

    pub fn as_owned(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            raw: self.raw,
        }
    }
}

impl<F: Borrow<SourceFile>> Eq for AnyFileLoc<F> {}
impl<F: Borrow<SourceFile>> PartialEq for AnyFileLoc<F> {
    fn eq(&self, other: &Self) -> bool {
        self.file() == other.file() && self.raw == other.raw
    }
}

impl<F: Borrow<SourceFile>> PartialOrd<Self> for AnyFileLoc<F> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.file() == other.file() {
            Some(self.raw.index.cmp(&other.raw.index))
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct FileLocRaw {
    pub index: usize,
    pub pos: FilePos,
}

impl FileLocRaw {
    pub const HEAD: Self = Self {
        index: 0,
        pos: FilePos::HEAD,
    };

    pub fn line(&self) -> usize {
        self.pos.line
    }

    pub fn col(&self) -> usize {
        self.pos.col
    }
}

impl Ord for FileLocRaw {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl PartialOrd for FileLocRaw {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct FilePos {
    pub line: usize,
    pub col: usize,
}

impl FilePos {
    pub const HEAD: Self = Self { line: 0, col: 0 };
}

impl Display for FilePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl Ord for FilePos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.col.cmp(&other.col),
            prim @ _ => prim,
        }
    }
}

impl PartialOrd for FilePos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// === Reader === //

pub trait Reader: Clone {
    type Output;

    fn consume(&mut self) -> Self::Output;

    fn peek(&self) -> Self::Output {
        self.clone().consume()
    }

    /// Attempts to match the reader sequence using the handler, committing the state if the return
    /// value is truthy (*e.g.* `true`, `Some(_)`, `Ok(_)`; see [LookaheadResult] for details) and
    /// ignoring all reader state changes otherwise.
    fn lookahead<F, R>(&mut self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        let mut lookahead = self.clone();
        let res = handler(&mut lookahead);
        if res.is_truthy() {
            *self = lookahead;
        }
        res.into_result()
    }

    fn peek_ahead<F, R>(&self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.clone()).into_result()
    }

    fn consume_while<F>(&mut self, mut handler: F)
    where
        F: FnMut(&mut Self) -> bool,
    {
        while self.lookahead(&mut handler) {}
    }
}

pub trait LookaheadResult {
    type Ret;

    fn is_truthy(&self) -> bool;
    fn into_result(self) -> Self::Ret;
}

impl LookaheadResult for bool {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        *self
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for Option<T> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_some()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_ok()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for (bool, T) {
    type Ret = T;

    fn is_truthy(&self) -> bool {
        self.0
    }

    fn into_result(self) -> Self::Ret {
        self.1
    }
}

pub trait AsFileReader {
    fn reader(&self) -> FileReader;
}

#[derive(Clone)]
pub struct FileReader<'a> {
    file: &'a SourceFile,
    reader: CharReader<'a>,
    next_pos: FilePos,
    prev_pos: FilePos,
    prev_idx: usize,
}

impl<'a> FileReader<'a> {
    fn new(file: &'a SourceFile, source: &'a [u8], loc_raw: FileLocRaw) -> Self {
        Self {
            file,
            reader: CharReader::new(source, loc_raw.index),
            next_pos: loc_raw.pos,
            prev_pos: loc_raw.pos,
            prev_idx: 0,
        }
    }

    fn consume_untracked(&mut self) -> ReadAtom {
        match self.reader.consume() {
            // Match CRLF
            Ok(Some('\r')) => {
                let valid = self
                    .reader
                    .lookahead(|reader| reader.consume() == Ok(Some('\n')));

                ReadAtom::Newline { valid }
            }

            // Match LF
            Ok(Some('\n')) => ReadAtom::Newline { valid: true },

            // Match char
            Ok(Some(char)) => ReadAtom::Codepoint(char),

            // Match EOF
            Ok(None) => ReadAtom::Eof,

            // Match read errors
            Err(CharReadErr::BadUnicode(code)) => ReadAtom::Unknown(code),
        }
    }

    /// Returns the location of the most recently read atom.
    pub fn prev_loc(&self) -> FileLocRef<'a> {
        FileLocRef {
            file: self.file,
            raw: FileLocRaw {
                index: self.reader.index() - 1,
                pos: self.next_pos,
            },
        }
    }

    /// Returns the location of the atom about to be consumed.
    pub fn next_loc(&self) -> FileLocRef<'a> {
        FileLocRef {
            file: self.file,
            raw: FileLocRaw {
                index: self.reader.index(),
                pos: self.next_pos,
            },
        }
    }
}

impl Reader for FileReader<'_> {
    type Output = ReadAtom;

    fn consume(&mut self) -> Self::Output {
        // Save previous state
        self.prev_pos = self.next_pos;
        self.prev_idx = self.reader.index();

        // Consume atom
        let result = self.consume_untracked();

        // Update positional state
        match result {
            // Most text editors seem to treat codepoints as characters.
            ReadAtom::Codepoint(_) => self.next_pos.col += 1,
            // These will probably show up as illegal character boxes in the editor.
            ReadAtom::Unknown(_) => self.next_pos.col += 1,
            // Newlines, valid or not, are typically treated as newlines by editors.
            ReadAtom::Newline { .. } => {
                self.next_pos.line += 1;
                self.next_pos.col = 0;
            }
            ReadAtom::Eof => {}
        }

        result
    }

    fn peek(&self) -> Self::Output {
        self.clone().consume_untracked()
    }
}

#[derive(Clone)]
struct CharReader<'a> {
    source: &'a [u8],
    index: usize,
}

impl<'a> CharReader<'a> {
    pub fn new(source: &'a [u8], index: usize) -> Self {
        Self { source, index }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl Reader for CharReader<'_> {
    type Output = Result<Option<char>, CharReadErr>;

    fn consume(&mut self) -> Self::Output {
        let mut stream = self.source.iter();

        // Consume char
        let start = stream.len();
        let code = next_code_point(&mut stream);

        // Decode char
        let char = match code {
            Some(code) => Some(char::from_u32(code).ok_or(CharReadErr::BadUnicode(code))?),
            None => None,
        };

        // Update stream
        self.source = stream.as_slice();
        self.index += start - stream.len();

        Ok(char)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum CharReadErr {
    BadUnicode(u32),
}

/// An abstraction over unicode that represents an indivisible unit of text. Characters are
/// categorized by how they are typically handled by code editors.
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum ReadAtom {
    /// Codepoints best correspond with the editor's notion of a character, even though some rich
    /// text renderers might disagree.
    Codepoint(char),

    /// These are invalid codepoints, which are probably rendered as their own character anyways.
    Unknown(u32),

    /// Anything that most editors will typically treat as newlines. These may or may not be
    /// properly formed. These will all be transformed into a "safer" `\n` representation before
    /// being displayed.
    Newline { valid: bool },

    /// The end of a file.
    Eof,
}

impl ReadAtom {
    pub const UNRECOGNIZED_CHAR: char = '�';

    /// Returns whether this atom is well formed.
    pub fn is_well_formed(self) -> bool {
        match self {
            // Valid atoms
            Self::Codepoint(_) => true,
            Self::Newline { valid: true } => true,
            Self::Eof => true,
            // Invalid atoms
            Self::Unknown(_) => false,
            Self::Newline { valid: false } => false,
        }
    }

    /// Returns whether this atom is treated as a newline by most code editors.
    pub fn is_newline_like(self) -> bool {
        match self {
            ReadAtom::Newline { .. } => true,
            _ => false,
        }
    }

    /// Converts the atom into a codepoint character for use in rendering or matching.
    /// All malformed atoms are converted into safe-to-display equivalents.
    ///
    /// The EOF is held in a distinct enum variant to all other characters.
    pub fn as_char_or_eof(self) -> CharOrEof {
        match self {
            // Valid patterns
            Self::Codepoint(char) => CharOrEof::Char(char),
            Self::Newline { .. } => CharOrEof::Char('\n'),
            Self::Eof => CharOrEof::Eof,

            // Invalid patterns
            Self::Unknown(_) => CharOrEof::Char(Self::UNRECOGNIZED_CHAR),
        }
    }

    /// Converts the atom into a codepoint character for use in rendering or matching.
    /// All malformed atoms are converted into safe-to-display equivalents.
    ///
    /// The EOF is transformed into the nul character (`\0`).
    pub fn as_char(self) -> char {
        self.as_char_or_eof().nul_eof()
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CharOrEof {
    Char(char),
    Eof,
}

impl CharOrEof {
    pub fn nul_eof(self) -> char {
        match self {
            Self::Char(char) => char,
            Self::Eof => '\0',
        }
    }
}

impl From<char> for CharOrEof {
    fn from(char: char) -> Self {
        CharOrEof::Char(char)
    }
}
