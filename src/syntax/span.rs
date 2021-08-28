use core::str::next_code_point;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::Path;
use std::str::{from_utf8, Utf8Error};
use std::sync::Arc;

// === Logical source === //

#[derive(Clone)]
pub struct SourceFile {
    arc: Arc<SourceFileInner>,
}

struct SourceFileInner {
    path: Box<Path>,
    bytes: Vec<u8>,
}

impl SourceFile {
    pub fn from_file(path: Box<Path>) -> io::Result<Self> {
        let bytes = fs::read(&path)?;
        Ok(Self::from_bytes(path, bytes))
    }

    pub fn from_bytes(path: Box<Path>, bytes: Vec<u8>) -> Self {
        Self {
            arc: Arc::new(SourceFileInner { path, bytes }),
        }
    }

    pub fn reader(&self) -> FileReader {
        FileReader::new(self, &self.arc.bytes, FilePos::HEAD)
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
        write!(f, "SourceFile {{ path: {:?}, .. }}", self.arc.path)
    }
}

pub type Span = AnySpan<SourceFile>;
pub type SpanRef<'a> = AnySpan<&'a SourceFile>;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct AnySpan<F> {
    file: F,
    start: FilePos,
    end: FilePos,
}

impl Span {
    pub fn new<F1, F2>(a: &AnyFileLoc<F1>, b: &AnyFileLoc<F2>) -> Self
    where
        F1: Borrow<SourceFile>,
        F2: Borrow<SourceFile>,
    {
        assert_eq!(
            a.file(),
            b.file(),
            "Cannot construct a Span spanning two different files!"
        );

        let file = a.file().clone();
        let mut locs = [a.pos, b.pos];
        locs.sort();

        Self {
            file,
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
        &self.file().arc.bytes
    }

    pub fn try_as_str(&self) -> Result<&str, Utf8Error> {
        from_utf8(self.bytes())
    }

    pub fn as_str(&self) -> &str {
        self.try_as_str().unwrap()
    }

    pub fn reader(&self) -> FileReader {
        FileReader::new(self.file(), self.bytes(), self.start)
    }

    pub fn start(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            pos: self.start,
        }
    }

    pub fn end(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            pos: self.end,
        }
    }
}

pub type FileLoc = AnyFileLoc<SourceFile>;
pub type FileLocRef<'a> = AnyFileLoc<&'a SourceFile>;

#[derive(Debug, Clone, Hash)]
pub struct AnyFileLoc<F> {
    file: F,
    pos: FilePos,
}

impl<F> AnyFileLoc<F> {
    pub fn pos(&self) -> FilePos {
        self.pos
    }
}

impl<F: Borrow<SourceFile>> AnyFileLoc<F> {
    pub fn file(&self) -> &SourceFile {
        self.file.borrow()
    }

    pub fn as_ref(&self) -> FileLocRef {
        FileLocRef {
            file: self.file(),
            pos: self.pos,
        }
    }

    pub fn as_owned(&self) -> FileLoc {
        FileLoc {
            file: self.file().clone(),
            pos: self.pos,
        }
    }
}

impl<F: Borrow<SourceFile>> Eq for AnyFileLoc<F> {}
impl<F: Borrow<SourceFile>> PartialEq for AnyFileLoc<F> {
    fn eq(&self, other: &Self) -> bool {
        self.file() == other.file() && self.pos == other.pos
    }
}

impl<F: Borrow<SourceFile>> PartialOrd<Self> for AnyFileLoc<F> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.file() == other.file() {
            Some(self.pos.index.cmp(&other.pos.index))
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct FilePos {
    pub index: usize,
    pub line: usize,
    pub col: usize,
}

impl FilePos {
    pub const HEAD: Self = Self {
        index: 0,
        line: 0,
        col: 0,
    };
}

impl Display for FilePos {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl Ord for FilePos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl PartialOrd for FilePos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// === Reader === //

#[derive(Clone)]
struct CharReader<'a> {
    source: &'a [u8],
    index: usize,
}

impl<'a> CharReader<'a> {
    pub fn new(source: &'a [u8], index: usize) -> Self {
        Self { source, index }
    }

    pub fn peek(&self) -> Result<Option<char>, CharReadErr> {
        self.clone().consume()
    }

    pub fn consume(&mut self) -> Result<Option<char>, CharReadErr> {
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

    pub fn index(&self) -> usize {
        self.index
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum CharReadErr {
    BadUnicode(u32),
}

#[derive(Clone)]
pub struct FileReader<'a> {
    file: &'a SourceFile,
    reader: CharReader<'a>,
    line: usize,
    col: usize,
}

impl<'a> FileReader<'a> {
    fn new(file: &'a SourceFile, source: &'a [u8], pos: FilePos) -> Self {
        Self {
            file,
            reader: CharReader::new(source, pos.index),
            line: pos.line,
            col: pos.col,
        }
    }

    fn consume_untracked(&mut self) -> ReadAtom {
        match self.reader.consume() {
            // Match CRLF
            Ok(Some('\r')) => match self.reader.consume() {
                Ok(Some('\n')) => ReadAtom::Newline { valid: true },
                _ => ReadAtom::Newline { valid: false },
            },

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

    pub fn peek(&self) -> ReadAtom {
        self.clone().consume_untracked()
    }

    pub fn consume(&mut self) -> ReadAtom {
        let result = self.consume_untracked();
        match result {
            // Most text editors seem to treat codepoints as characters.
            ReadAtom::Codepoint(_) => self.col += 1,
            // These will probably show up as illegal character boxes in the editor.
            ReadAtom::Unknown(_) => self.col += 1,
            ReadAtom::Newline { .. } => {
                self.line += 1;
                self.col = 0;
            }
            ReadAtom::Eof => {}
        }

        result
    }

    pub fn loc(&self) -> FileLocRef<'a> {
        FileLocRef {
            file: self.file,
            pos: FilePos {
                index: self.reader.index(),
                line: self.line,
                col: self.col,
            },
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum ReadAtom {
    Codepoint(char),
    Unknown(u32),
    Newline { valid: bool },
    Eof,
}

impl ReadAtom {
    pub fn is_newline(&self) -> bool {
        match self {
            ReadAtom::Newline { .. } => true,
            _ => false,
        }
    }

    pub fn as_char(&self) -> Option<CharOrEof> {
        match *self {
            // Valid patterns
            ReadAtom::Codepoint(char) => Some(CharOrEof::Char(char)),
            ReadAtom::Newline { valid: true } => Some(CharOrEof::Char('\n')),
            ReadAtom::Eof => Some(CharOrEof::Eof),

            // Invalid patterns
            ReadAtom::Unknown(_) => None,
            ReadAtom::Newline { valid: false } => None,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CharOrEof {
    Char(char),
    Eof,
}