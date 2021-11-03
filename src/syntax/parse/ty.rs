// TODO: Function types with full generic support and infix "type operators" (e.g. "?", "[]", "*" if we do move semantics)

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
    pub unnamed_params: Vec<AstType>,
    pub named_params: Vec<(String, AstType)>,
}

impl AstTypeObjGenerics {
    // FIXME: We'll actually fix this warning once proper list matching infrastructure is in place.
    //noinspection DuplicatedCode
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match '<'
            let _ = util_match_punct(reader, PunctChar::Less, None)?;

            // Match list
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));
            let unnamed_params = {
                let mut collector = Vec::new();

                reader.consume_while(|reader| {
                    // Match delimiter
                    if delimited.next(reader).is_none() {
                        return false;
                    }

                    // Match param type
                    if let Some(parsed) = AstType::parse(reader) {
                        collector.push(parsed);
                        true
                    } else {
                        false
                    }
                });
                collector
            };

            let named_params = {
                let mut collector = Vec::new();

                reader.consume_while(|reader| {
                    // Match delimiter
                    if delimited.next(reader).is_none() {
                        return false;
                    }

                    // Match param name
                    let name = match util_match_ident(reader) {
                        Some(id) => &id.text,
                        None => return false,
                    };

                    // Match equals
                    if util_match_punct(reader, PunctChar::Equals, None).is_none() {
                        return false;
                    }

                    // Match param type
                    if let Some(parsed) = AstType::parse(reader) {
                        collector.push((name.clone(), parsed));
                        true
                    } else {
                        false
                    }
                });
                collector
            };

            // Match '>'
            let _ = util_match_punct(reader, PunctChar::Greater, None)?;

            // Produce
            Some(Self {
                unnamed_params,
                named_params,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstTypeTuple {
    pub types: Vec<AstType>,
}

impl AstTypeTuple {
    // FIXME: Also fix here.
    //noinspection DuplicatedCode
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
            if !inner_reader.consume().is_some() {
                return None;
            }

            Some(Self { types })
        })
    }
}
