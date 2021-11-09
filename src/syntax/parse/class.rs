use crate::syntax::parse::path::AstPathTree;
use crate::syntax::parse::ty::AstType;
use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_specific_kw, util_punct_matcher, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, StreamReader};

#[derive(Debug, Clone)]
pub struct AstClassItem {
    pub qualifiers: Vec<AstClassQualifier>,
    pub kind: AstClassItemKind,
}

// === Item kinds === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstClassItemKind {

    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemBlock {}

// === Qualifiers === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstClassQualifier {
        In(AstClassQualifierIn),
        Out(AstClassQualifierOut),
        // Impl(AstClassQualifierImpl),
        // Where(AstClassQualifierWhere),
        Static(AstClassQualifierStatic),
    }
}

impl AstClassQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstClassQualifierIn::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierOut::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierStatic::parse(reader)?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstClassQualifierIn {
    pub visible_to: Vec<AstPathTree>,
}

impl AstClassQualifierIn {
    //noinspection DuplicatedCode
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            if !util_match_specific_kw(reader, AstKeyword::In) {
                return None;
            }

            let visible_to = AstPathTree::parse_path_parens(reader)?;

            Some(Self { visible_to })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstClassQualifierOut {
    pub as_targets: Vec<AstOutTarget>,
}

impl AstClassQualifierOut {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match start keyword
            if !util_match_specific_kw(reader, AstKeyword::Out) {
                return None;
            }

            // Match optional type list
            // Empty lists and the lack thereof are treated identically by the compiler (i.e. it will
            // treat it as a single hole).
            let mut as_targets = Vec::new();
            if let Some(paren) = util_match_group_delimited(reader, GroupDelimiter::Paren) {
                let mut reader = paren.reader();
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                // Match main list
                reader.consume_while(|reader| {
                    // Match delimiter
                    if !delimited.next(reader).is_some() {
                        return false;
                    }

                    if let Some(target) = AstOutTarget::parse(reader) {
                        as_targets.push(target);
                        true
                    } else {
                        false
                    }
                });

                // Match EOF
                if reader.consume().is_some() {
                    return None;
                }
            }

            // Construct qualifier
            Some(Self { as_targets })
        })
    }
}

#[derive(Debug, Clone)]
pub enum AstOutTarget {
    Type(AstType),
    Infer,
    Dynamic,
}

impl AstOutTarget {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            // Match types
            |reader| Some(Self::Type(AstType::parse(reader)?)),
            // Match inference holes
            |reader| util_match_specific_kw(reader, AstKeyword::Hole).then_some(Self::Infer),
            // Match dynamic
            |reader| util_match_specific_kw(reader, AstKeyword::Dynamic).then_some(Self::Dynamic)
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstClassQualifierImpl {}

#[derive(Debug, Clone)]
pub struct AstClassQualifierWhere {}

#[derive(Debug, Clone)]
pub struct AstClassQualifierStatic;

impl AstClassQualifierStatic {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        util_match_specific_kw(reader, AstKeyword::Static).then_some(Self)
    }
}
