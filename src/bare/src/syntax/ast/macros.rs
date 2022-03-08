use crate::syntax::ast::path::AstPathDirect;
use crate::syntax::ast::util::{util_match_group, util_match_punct, AstCx};
use crate::syntax::token::ir::{PunctChar, TokenGroup};
use crate::util::reader::LookaheadReader;

#[derive(Debug, Clone)]
pub struct AstAnyAttrMacro {
    pub path: AstPathDirect,
    pub arg: Option<TokenGroup>,
}

impl AstAnyAttrMacro {
    pub fn parse((cx, reader): AstCx) -> Option<(bool, Self)> {
        reader.lookahead(|reader| {
            // Match @ symbol
            util_match_punct(reader, PunctChar::At, None)?;

            // Match ! symbol
            let is_inner = util_match_punct(reader, PunctChar::Exclamation, None).is_some();

            // Match path to target attribute
            let path = AstPathDirect::parse((cx, reader))?;

            // Match optional opaque token group passed directly to the macro.
            let arg = util_match_group(reader).map(Clone::clone);

            Some((is_inner, Self { path, arg }))
        })
    }

    pub fn parse_inner((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| match Self::parse((cx, reader)) {
            Some((true, attr)) => Some(attr),
            _ => None,
        })
    }

    pub fn parse_outer((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| match Self::parse((cx, reader)) {
            Some((false, attr)) => Some(attr),
            _ => None,
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstAttrQualifier {
    pub attr: AstAnyAttrMacro,
}

impl AstAttrQualifier {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        Some(Self {
            attr: AstAnyAttrMacro::parse_outer((cx, reader))?,
        })
    }
}
