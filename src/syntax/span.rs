use crate::util::backing::Take;
use crate::util::reader::{LookaheadReader, StreamReader, StreamResult};
use core::str::next_code_point;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::str::{from_utf8, Utf8Error};
use std::sync::Arc;
use thiserror::Error;

// === Logical source === //

/// A source file from a given path. Has [Arc] clone semantics.
#[derive(Clone)]
pub struct SourceFile {
    arc: Arc<SourceFileInner>,
}

struct SourceFileInner {
    path: PathBuf,
    bytes: Vec<u8>,
}

impl SourceFile {
    pub fn dummy() -> (Self, Span, FileLoc) {
        let file = SourceFile::from_bytes("DUMMY_FILE".into(), Vec::new());
        let loc = file.head_loc();
        let span = Span::new(&loc, &loc);
        (file, span, loc)
    }

    pub fn from_str(path: &str, contents: String) -> Self {
        Self::from_bytes(path.into(), contents.into_bytes())
    }

    pub fn from_file(path: PathBuf) -> io::Result<Self> {
        let bytes = fs::read(&path)?;
        Ok(Self::from_bytes(path, bytes))
    }

    pub fn from_bytes(path: PathBuf, bytes: Vec<u8>) -> Self {
        Self {
            arc: Arc::new(SourceFileInner { path, bytes }),
        }
    }

    pub fn head_loc(&self) -> FileLoc {
        FileLoc {
            file: self.clone(),
            raw: FileLocRaw::HEAD,
        }
    }

    pub fn path(&self) -> &Path {
        self.arc.path.as_path()
    }

    pub fn bytes(&self) -> &[u8] {
        &self.arc.bytes
    }

    pub fn reader(&self) -> FileReader {
        FileReader::new(self, &self.arc.bytes, FileLocRaw::HEAD)
    }

    fn marshall_name(&self) -> &str {
        self.arc.path.to_str().unwrap_or("???")
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

/// An inclusive span of characters in a [SourceFile].
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Span {
    file: SourceFile,
    start: FileLocRaw,
    end: FileLocRaw,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if self.start.pos.line != self.end.pos.line {
            write!(
                f,
                "{}:{}-{}",
                self.file.marshall_name(),
                self.start.pos,
                self.end.pos,
            )
        } else {
            write!(f, "{}:{}", self.file.marshall_name(), self.start.pos)
        }
    }
}

impl Span {
    pub fn new<A: Take<FileLoc>, B: Take<FileLoc>>(a: A, b: B) -> Self {
        let (a, b) = (a.take(), b.as_ref());

        assert_eq!(
            a.file(),
            b.file(),
            "Cannot construct a Span spanning two different files!"
        );

        let mut locs = [a.raw, b.raw];
        locs.sort();

        Self {
            file: a.file,
            start: locs[0],
            end: locs[1],
        }
    }
}

impl Span {
    pub fn file(&self) -> &SourceFile {
        &self.file
    }

    pub fn bytes(&self) -> &[u8] {
        &self.file.arc.bytes[self.start.index..=self.end.index]
    }

    pub fn try_as_str(&self) -> Result<&str, Utf8Error> {
        from_utf8(self.bytes())
    }

    pub fn as_str(&self) -> &str {
        self.try_as_str().unwrap()
    }

    pub fn start(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            raw: self.start,
        }
    }

    pub fn start_pos(&self) -> FilePos {
        self.start.pos
    }

    pub fn end(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            raw: self.end,
        }
    }

    pub fn end_pos(&self) -> FilePos {
        self.end.pos
    }

    fn set_pair_unchecked(&mut self, a: FileLocRaw, b: FileLocRaw) {
        let mut locs = [a, b];
        locs.sort();
        self.start = locs[0];
        self.end = locs[1];
    }

    pub fn set_start<L: Take<FileLoc>>(&mut self, loc: L) {
        let loc = loc.as_ref();
        assert_eq!(
            loc.file(),
            self.file(),
            "Cannot set span start from a different file!"
        );

        self.set_pair_unchecked(loc.raw, self.end);
    }

    pub fn set_end<L: Take<FileLoc>>(&mut self, loc: L) {
        let loc = loc.as_ref();
        assert_eq!(
            loc.file(),
            self.file(),
            "Cannot set span end from a different file!"
        );

        self.set_pair_unchecked(self.start, loc.raw);
    }

    pub fn reader(&self) -> FileReader {
        FileReader::new(self.file(), self.bytes(), self.start)
    }
}

/// A location in a file.
#[derive(Debug, Clone, Hash)]
pub struct FileLoc {
    file: SourceFile,
    raw: FileLocRaw,
}

impl FileLoc {
    pub fn line_start_pos(&self) -> FilePos {
        FilePos {
            line: self.raw.pos.line,
            col: 0,
        }
    }

    pub fn line_start_index(&self) -> usize {
        self.raw.line_begin
    }

    pub fn line_start(&self) -> Self {
        Self {
            file: self.file.clone(),
            raw: FileLocRaw {
                line_begin: self.line_start_index(),
                index: self.line_start_index(),
                pos: self.line_start_pos(),
            },
        }
    }

    pub fn pos(&self) -> FilePos {
        self.raw.pos
    }

    pub fn char_index(&self) -> usize {
        self.raw.index
    }

    pub fn displayed_line(&self) -> usize {
        self.raw.pos.displayed_line()
    }

    pub fn displayed_col(&self) -> usize {
        self.raw.pos.displayed_col()
    }

    pub fn file(&self) -> &SourceFile {
        self.file.borrow()
    }

    pub fn reader(&self) -> FileReader {
        FileReader::new(
            self.file(),
            &self.file.bytes()[self.char_index()..],
            self.raw,
        )
    }

    pub fn as_span(&self) -> Span {
        Span::new(self, self)
    }
}

impl Eq for FileLoc {}
impl PartialEq for FileLoc {
    fn eq(&self, other: &Self) -> bool {
        self.file() == other.file() && self.raw == other.raw
    }
}

impl PartialOrd<Self> for FileLoc {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.file() == other.file() {
            Some(self.raw.cmp(&other.raw))
        } else {
            None
        }
    }
}

impl Display for FileLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(
            f,
            "{}:{}:{}",
            self.file.marshall_name(),
            self.displayed_line(),
            self.displayed_col(),
        )
    }
}

#[derive(Debug, Copy, Clone, Hash)]
struct FileLocRaw {
    pub index: usize,
    pub line_begin: usize,
    pub pos: FilePos,
}

impl FileLocRaw {
    pub const HEAD: Self = Self {
        index: 0,
        line_begin: 0,
        pos: FilePos::HEAD,
    };
}

impl Eq for FileLocRaw {}

impl PartialEq for FileLocRaw {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
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

    pub fn displayed_line(&self) -> usize {
        self.line + 1
    }

    pub fn displayed_col(&self) -> usize {
        self.col + 1
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}:{}", self.displayed_line(), self.displayed_col())
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

// === Readers === //

#[derive(Clone)]
struct CharReader<'a> {
    /// The remaining byte stream. [consume](CharReader::consume) reads from the beginning of this
    /// slice.
    source: &'a [u8],

    /// The index of the next character assuming it were to exist. Use [next_index] to obtain an
    /// actually valid index.
    index: usize,
}

impl<'a> CharReader<'a> {
    pub fn new(source: &'a [u8], index: usize) -> Self {
        Self { source, index }
    }

    pub fn next_index(&self) -> usize {
        if self.source.len() > 0 {
            self.index
        } else {
            self.index - 1
        }
    }
}

impl<'a> StreamReader for CharReader<'a> {
    type Res = Result<Option<char>, CharReadErr>;

    fn consume(&mut self) -> Self::Res {
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

impl LookaheadReader for CharReader<'_> {}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Error)]
enum CharReadErr {
    #[error("Invalid unicode codepoint {0:#X}")]
    BadUnicode(u32),
}

#[derive(Clone)]
pub struct FileReader<'a> {
    /// The file we're reading from.
    file: &'a SourceFile,

    /// The underlying character stream reader. Some atoms may be formed of more than one Unicode
    /// codepoint.
    reader: CharReader<'a>,

    /// The location of the next atom to be returned through [consume](FileReader::consume).
    next_pos: FileLocRaw,

    /// The location of the latest atom to be returned by [consume](FileReader::consume). Defaults to
    /// the starting position if nothing has been read yet.
    prev_pos: FileLocRaw,
}

impl<'a> FileReader<'a> {
    fn new(file: &'a SourceFile, source: &'a [u8], loc_raw: FileLocRaw) -> Self {
        Self {
            file,
            reader: CharReader::new(source, loc_raw.index),
            next_pos: loc_raw,
            prev_pos: loc_raw,
        }
    }

    /// Consumes from the underlying character `reader` without updating the [FileReader]'s state.
    fn consume_untracked(&mut self) -> ReadAtom {
        match self.reader.consume() {
            // Match CRLF
            Ok(Some('\r')) => {
                let valid = self
                    .reader
                    .lookahead(|reader| reader.consume() == Ok(Some('\n')));

                ReadAtom::Newline(if valid {
                    NewlineKind::Crlf
                } else {
                    NewlineKind::Malformed
                })
            }

            // Match LF
            Ok(Some('\n')) => ReadAtom::Newline(NewlineKind::Lf),

            // Match char
            Ok(Some(char)) => ReadAtom::Codepoint(char),

            // Match EOF
            Ok(None) => ReadAtom::Eof,

            // Match read errors
            Err(CharReadErr::BadUnicode(code)) => ReadAtom::Unknown(code),
        }
    }

    /// Returns the location of the most recently read atom.
    pub fn prev_loc(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            raw: self.prev_pos,
        }
    }

    /// Returns the location of the atom about to be consumed.
    pub fn next_loc(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            raw: self.next_pos,
        }
    }

    pub fn file(&self) -> &'a SourceFile {
        self.file
    }
}

impl StreamReader for FileReader<'_> {
    type Res = ReadAtom;

    fn consume(&mut self) -> Self::Res {
        // Save previous state
        self.prev_pos = self.next_pos;

        // Consume atom
        let result = self.consume_untracked();
        self.next_pos.index = self.reader.next_index();

        // Update positional state
        match result {
            // Most text editors seem to treat codepoints as characters.
            ReadAtom::Codepoint(_) => self.next_pos.pos.col += 1,
            // These will probably show up as illegal character boxes in the editor.
            ReadAtom::Unknown(_) => self.next_pos.pos.col += 1,
            // Newlines, valid or not, are typically treated as newlines by editors.
            ReadAtom::Newline(_) => {
                self.next_pos.pos.line += 1;
                self.next_pos.pos.col = 0;
                self.next_pos.line_begin = self.next_pos.index;
            }
            ReadAtom::Eof => {}
        }

        result
    }

    fn peek(&self) -> ReadAtom {
        self.clone().consume_untracked()
    }
}

impl LookaheadReader for FileReader<'_> {}

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
    Newline(NewlineKind),

    /// The end of a file.
    Eof,
}

impl ReadAtom {
    pub const UNRECOGNIZED_CHAR: char = '�';

    /// Returns whether this atom is well formed.
    pub fn is_well_formed(self) -> bool {
        match self {
            Self::Codepoint(_) => true,
            Self::Eof => true,
            Self::Newline(kind) => kind.is_well_formed(),
            Self::Unknown(_) => false,
        }
    }

    /// Returns whether this atom is treated as a newline by most code editors.
    pub fn is_newline_like(self) -> bool {
        match self {
            ReadAtom::Newline(_) => true,
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
            Self::Newline(_) => CharOrEof::Char('\n'),
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

impl StreamResult for ReadAtom {
    type Item = ReadAtom;

    fn to_item(self) -> Option<Self::Item> {
        if self != ReadAtom::Eof {
            Some(self)
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum NewlineKind {
    Lf,
    Crlf,
    Malformed,
}

impl NewlineKind {
    pub fn is_well_formed(self) -> bool {
        match self {
            NewlineKind::Lf => true,
            NewlineKind::Crlf => true,
            NewlineKind::Malformed => false,
        }
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
