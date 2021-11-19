use crate::syntax::parse::path::AstPathDirect;
use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_ident, util_match_punct, util_punct_matcher,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, StreamReader};

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstType {
        Obj(AstTypeObj),
        Tup(AstTypeTuple),
    }
}

impl AstType {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstTypeObj::parse(reader)?.wrap()),
            |reader| Some(AstTypeTuple::parse(reader)?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeObj {
    pub path: AstPathDirect,
    pub generics: Option<AstTypeObjGenerics>,
}

impl AstTypeObj {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let path = AstPathDirect::parse(reader)?;
            let generics = AstTypeObjGenerics::parse(reader);

            Some(Self { path, generics })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeObjGenerics {
    pub params: Vec<GenericParamKind>,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind {
    Named(String, AstType),
    Unnamed(AstType),
}

impl AstTypeObjGenerics {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match '<'
            let _ = util_match_punct(reader, PunctChar::Less, None)?;

            // Match list
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));
            let params = reader
                .consume_while(|reader| {
                    let _ = delimited.next(reader)?;
                    Self::parse_param(reader)
                })
                .collect();

            // Match '>'
            let _ = util_match_punct(reader, PunctChar::Greater, None)?;

            // Produce
            Some(Self { params })
        })
    }

    pub fn parse_param(reader: &mut TokenStreamReader) -> Option<GenericParamKind> {
        // N.B. we match named in a different priority group to unnamed because the unnamed grammar
        // is a partial subset of named but not vice-versa.
        match_choice!(
            reader,
            // Match named
            [|reader| {
                // Match param name
                let name = util_match_ident(reader)?;

                // Match equals
                let _ = util_match_punct(reader, PunctChar::Equals, None)?;

                // Match param type
                let ty = AstType::parse(reader)?;

                Some(GenericParamKind::Named(name.take_text(), ty))
            }],
            // Match unnamed
            [|reader| { Some(GenericParamKind::Unnamed(AstType::parse(reader)?)) }],
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeTuple {
    pub types: Vec<AstType>,
}

impl AstTypeTuple {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let paren = util_match_group_delimited(reader, GroupDelimiter::Paren)?;
            let mut inner_reader = paren.reader();
            let mut types = Vec::new();
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

            // Match list
            inner_reader.consume_while(|inner_reader| {
                // Match delimiter
                if delimited.next(inner_reader).is_none() {
                    return false;
                }

                // Match type
                if let Some(ty) = AstType::parse(inner_reader) {
                    types.push(ty);
                    true
                } else {
                    false
                }
            });

            // Match EOF
            if !inner_reader.consume().is_none() {
                return None;
            }

            Some(Self { types })
        })
    }
}
