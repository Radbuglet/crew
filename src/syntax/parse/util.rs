use crate::syntax::span::SpanRef;
use crate::syntax::token::{
    GroupDelimiter, PunctChar, TokenGroup, TokenIdent, TokenPunct, TokenStreamReader,
};
use crate::util::enum_utils::*;
use crate::util::reader::{LookaheadReader, StreamReader};

pub const TURBO: &'static [PunctChar] = &[PunctChar::Colon, PunctChar::Colon];

enum_meta! {
    #[derive(Debug)]
    pub enum(&'static str) AstKeyword {
        Pub = "pub",
        Crate = "crate",
        Self_ = "self",
        As = "as",
        Class = "class",
        Enum = "enum",
        Fn = "fn",
        Static = "static",
        Macro = "macro",
        Use = "use",
        Mod = "mod",
        If = "if",
        Loop = "loop",
        While = "while",
        Match = "match",
        Break = "break",
        Continue = "continue",
        Return = "return",
        Open = "open",
        In = "in",
        Out = "out",
        Impl = "impl",
        Readonly = "readonly",
        Val = "val",
        Var = "var",

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
        Super = "super",
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdentOrKw<'a> {
    pub raw: &'a TokenIdent,
    pub kw: Option<AstKeyword>,
}

pub fn util_match_ident_or_kw<'a>(reader: &mut TokenStreamReader<'a>) -> Option<IdentOrKw<'a>> {
    reader.lookahead(|reader| {
        let raw = reader.consume()?.try_cast_ref::<TokenIdent>()?;
        let kw = AstKeyword::find_where(|_, text| *text == raw.text.as_str());
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

pub fn util_match_specific_kw(reader: &mut TokenStreamReader, kw: AstKeyword) -> bool {
    reader.lookahead(|reader| util_match_kw(reader) == Some(kw))
}

pub fn util_match_group<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenGroup> {
    reader.lookahead(|reader| Some(reader.consume()?.try_cast_ref::<TokenGroup>()?))
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
        let punct = reader.consume()?.try_cast_ref::<TokenPunct>()?;
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
) -> Option<SpanRef<'a>> {
    assert!(!seq.is_empty());

    reader.lookahead(|reader| {
        let mut span = reader.next_span()?;
        let mut glued = None;
        for char in seq {
            if util_match_punct(reader, *char, glued).is_none() {
                return None;
            }
            glued = Some(true);
        }
        span.set_end(&reader.last_loc().unwrap());
        Some(span)
    })
}

pub fn util_match_turbo<'a>(reader: &mut TokenStreamReader<'a>) -> Option<SpanRef<'a>> {
    util_match_punct_seq(reader, TURBO)
}
