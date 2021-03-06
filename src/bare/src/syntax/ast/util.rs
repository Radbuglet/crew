//! Utilities used throughout the parser. Many fundamental objects are included here rather than in
//! the `parse` module to avoid leaking imports to descendant modules.

use crate::syntax::diagnostic::Diagnostics;
use crate::syntax::span::Span;
use crate::syntax::token::ir::{
    GroupDelimiter, PunctChar, TokenGroup, TokenIdent, TokenNumberLit, TokenPunct,
    TokenStreamReader, TokenStringLit,
};
use crate::util::enum_utils::*;
use crate::util::reader::{LookaheadReader, StreamReader};
use std::fmt::{Debug, Display, Formatter};

// === Parser context === //

pub type AstCx<'r, 'a> = (&'r mut AstCxBundle, &'r mut TokenStreamReader<'a>);

#[derive(Debug, Clone)]
pub struct AstCxBundle {
    pub diag: Diagnostics,
}

// === Primitive token matching === //

pub fn util_match_group<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenGroup> {
    reader.lookahead(|reader| Some(reader.consume().ok()?.try_cast_ref::<TokenGroup>()?))
}

pub fn util_match_str_lit<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenStringLit> {
    reader.lookahead(|reader| Some(reader.consume().ok()?.try_cast_ref::<TokenStringLit>()?))
}

pub fn util_match_num_lit<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenNumberLit> {
    reader.lookahead(|reader| Some(reader.consume().ok()?.try_cast_ref::<TokenNumberLit>()?))
}

pub fn util_match_eof(reader: &mut TokenStreamReader) -> Option<()> {
    reader.lookahead(|reader| reader.consume().is_err().then_some(()))
}

pub fn util_match_group_delimited<'a>(
    reader: &mut TokenStreamReader<'a>,
    expected_delimiter: GroupDelimiter,
) -> Option<&'a TokenGroup> {
    reader.lookahead(|reader| match util_match_group(reader) {
        Some(group @ TokenGroup { delimiter: del, .. }) if *del == expected_delimiter => {
            Some(group)
        }
        _ => None,
    })
}

pub fn util_match_punct<'a>(
    reader: &mut TokenStreamReader<'a>,
    char: PunctChar,
    glued: Option<bool>,
) -> Option<&'a TokenPunct> {
    reader.lookahead(|reader| {
        let punct = reader.consume().ok()?.try_cast_ref::<TokenPunct>()?;
        if punct.char == char && glued.map_or(true, |expected| expected == punct.is_glued) {
            Some(punct)
        } else {
            None
        }
    })
}

pub fn util_match_punct_seq<'a>(
    reader: &mut TokenStreamReader<'a>,
    seq: &[PunctChar],
) -> Option<Span> {
    assert!(!seq.is_empty());

    reader.lookahead(|reader| {
        let start = reader.next_loc();
        let mut glued = None;
        for char in seq {
            if util_match_punct(reader, *char, glued).is_none() {
                return None;
            }
            glued = Some(true);
        }

        Some(Span::new(start, reader.prev_loc()))
    })
}

pub fn util_punct_seq_matcher<'a>(
    seq: &'a [PunctChar],
) -> impl Fn(&mut TokenStreamReader) -> Option<Span> + 'a {
    move |reader| util_match_punct_seq(reader, seq)
}

pub fn util_punct_matcher(punct: PunctChar) -> impl Fn(&mut TokenStreamReader) -> Option<Span> {
    move |reader| util_match_punct_seq(reader, &[punct])
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenLitSeq<'a> {
    Punct(&'a [PunctChar]),
    Kw(AstKeyword),
}

impl TokenLitSeq<'_> {
    pub fn match_list<I, V>(reader: &mut TokenStreamReader, choices: I) -> Option<V>
    where
        I: IntoIterator<Item = (Self, V)>,
    {
        for (seq, ret) in choices {
            if seq.parse(reader).is_some() {
                return Some(ret);
            }
        }
        None
    }

    pub fn parse(self, reader: &mut TokenStreamReader) -> Option<Span> {
        match self {
            TokenLitSeq::Punct(seq) => util_match_punct_seq(reader, seq),
            TokenLitSeq::Kw(kw) => util_match_specific_kw(reader, kw).cloned(),
        }
    }
}

// === Punct sequence matching === //

pub const TURBO: &'static [PunctChar] = &[PunctChar::Colon, PunctChar::Colon]; // `::`
pub const FUNC_ARROW: &'static [PunctChar] = &[PunctChar::Dash, PunctChar::Greater]; // `->`
pub const ELLIPSIS: &'static [PunctChar] =
    &[PunctChar::Period, PunctChar::Period, PunctChar::Period]; // `...`

pub fn util_match_turbo(reader: &mut TokenStreamReader) -> Option<Span> {
    util_match_punct_seq(reader, TURBO)
}

pub fn util_match_func_arrow(reader: &mut TokenStreamReader) -> Option<Span> {
    util_match_punct_seq(reader, FUNC_ARROW)
}

pub fn util_match_ellipsis(reader: &mut TokenStreamReader) -> Option<Span> {
    util_match_punct_seq(reader, ELLIPSIS)
}

// === Keyword matching === //

enum_meta! {
    #[derive(Debug)]
    pub enum(&'static str) AstKeyword {
        /// Visibility specifier
        Pub = "pub",

        /// Path root specifier
        Crate = "crate",

        /// Path root and self type specifier
        SelfTy = "Self",

        /// `self` class reference specifier
        Self_ = "self",

        /// An alternative to `^` for specifying parent modules in paths
        Super = "super",

        /// Path renaming, expression casting
        As = "as",

        /// Module item qualifier
        Class = "class",

        /// Module item qualifier
        Enum = "enum",

        /// Module item qualifier
        Fn = "fn",

        /// Module item qualifier
        Macro = "macro",

        /// Module item qualifier
        Use = "use",

        /// Module item qualifier
        Mod = "mod",

        /// Module item qualifier
        Type = "type",

        /// Variable qualifier
        Static = "static",

        /// Expression keyword
        If = "if",
        Else = "else",

        /// Expression keyword
        Loop = "loop",

        /// Expression keyword
        While = "while",

        /// Expression keyword
        Match = "match",

        /// Expression keyword
        Break = "break",

        /// Expression keyword
        Continue = "continue",

        /// Expression keyword
        Return = "return",

        /// Expression keyword, class item.
        Val = "val",

        /// Expression keyword, class item.
        Var = "var",

        /// Class item qualifier
        Open = "open",

        /// Class item qualifier
        In = "in",

        /// Class item qualifier
        Out = "out",

        /// Class item qualifier
        Impl = "impl",

        /// Class item qualifier
        On = "on",

        /// Inference hole for types, unused parameter marker
        Hole = "_",

        /// Used to specify a dynamically determined component
        Dynamic = "dynamic",

        // === Reserved === //
        Abstract = "abstract",
        Final = "final",
        Sealed = "sealed",
        Ref = "ref",
        Box = "box",
        Gc = "gc",
        Rc = "rc",
        Delete = "delete",
        Await = "await",
        Async = "async",
        Yield = "yield",
        Override = "override",
        Readonly = "readonly",
    }
}

impl Display for AstKeyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.meta())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentOrKw<'a> {
    pub raw: &'a TokenIdent,
    pub kw: Option<AstKeyword>,
}

pub fn util_decode_keyword(name: &str) -> Option<AstKeyword> {
    AstKeyword::find_where(|_, text| *text == name)
}

pub fn util_match_ident_or_kw<'a>(reader: &mut TokenStreamReader<'a>) -> Option<IdentOrKw<'a>> {
    reader.lookahead(|reader| {
        let raw = reader.consume().ok()?.try_cast_ref::<TokenIdent>()?;
        let kw = util_decode_keyword(raw.text.as_str());
        Some(IdentOrKw { raw, kw })
    })
}

pub fn util_match_ident<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenIdent> {
    reader.lookahead(|reader| match util_match_ident_or_kw(reader) {
        Some(IdentOrKw { raw, kw: None }) => Some(raw),
        _ => None,
    })
}

pub fn util_match_kw(reader: &mut TokenStreamReader) -> Option<AstKeyword> {
    reader.lookahead(|reader| Some(util_match_ident_or_kw(reader)?.kw?))
}

pub fn util_match_specific_kw<'a>(
    reader: &mut TokenStreamReader<'a>,
    kw: AstKeyword,
) -> Option<&'a Span> {
    reader.lookahead(|reader| {
        let ident = reader.consume().ok()?.try_cast_ref::<TokenIdent>()?;
        if ident.text == *kw.meta() {
            Some(&ident.span)
        } else {
            None
        }
    })
}
