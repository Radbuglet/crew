use crate::syntax::ast::path::AstPathDirect;
use crate::syntax::ast::util::{
    util_match_eof, util_match_group_delimited, util_match_ident, util_match_punct,
    util_punct_matcher, AstCx,
};
use crate::syntax::token::ir::{GroupDelimiter, PunctChar};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{DelimiterMatcher, LookaheadReader};

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstType {
        Obj(AstTypeObj),
        Tup(AstTypeTuple),
        Never(AstTypeNever),
    }
}

impl AstType {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader
            .branch()
            .case(|reader| Some(AstTypeObj::parse((cx, reader))?.wrap()))
            .case(|reader| Some(AstTypeTuple::parse((cx, reader))?.wrap()))
            .case(|reader| Some(AstTypeNever::parse((cx, reader))?.wrap()))
            .done()
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeObj {
    pub path: AstPathDirect,
    pub generics: Option<AstTypeObjGenerics>,
}

impl AstTypeObj {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            let path = AstPathDirect::parse((cx, reader))?;
            let generics = AstTypeObjGenerics::parse((cx, reader))?;

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
    pub fn parse((cx, reader): AstCx) -> Option<Option<Self>> {
        reader.lookahead(|reader| {
            // Match '<'
            if util_match_punct(reader, PunctChar::Less, None).is_some() {
                // Match list
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                let params = reader
                    .consume_while(|reader| {
                        delimited.next(reader)?;
                        Self::parse_param((cx, reader))
                    })
                    .collect();

                // Match '>'
                util_match_punct(reader, PunctChar::Greater, None)?;

                // Produce
                Some(Some(Self { params }))
            } else {
                Some(None)
            }
        })
    }

    pub fn parse_param((cx, reader): AstCx) -> Option<GenericParamKind> {
        // N.B. we match named in a higher priority group than unnamed because the unnamed grammar
        // is a partial subset of named but not vice-versa.
        reader
            .branch()
            //> Match named
            .case(|reader| {
                // Match param name
                let name = util_match_ident(reader)?;

                // Match equals
                util_match_punct(reader, PunctChar::Equals, None)?;

                // Match param type
                let ty = AstType::parse((cx, reader))?;

                Some(GenericParamKind::Named(name.text(), ty))
            })
            //> Match unnamed
            .barrier()
            .case(|reader| Some(GenericParamKind::Unnamed(AstType::parse((cx, reader))?)))
            .done()
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeTuple {
    pub types: Vec<AstType>,
}

impl AstTypeTuple {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            let paren = util_match_group_delimited(reader, GroupDelimiter::Paren)?;
            let mut reader = paren.reader();
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

            // Match list
            let types = reader
                .consume_while(|reader| {
                    delimited.next(reader)?;
                    AstType::parse((cx, reader))
                })
                .collect();

            // Match EOF
            util_match_eof(&mut reader)?;

            Some(Self { types })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeNever;

impl AstTypeNever {
    pub fn parse((_cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_punct(reader, PunctChar::Exclamation, None)?;
            Some(AstTypeNever)
        })
    }
}
