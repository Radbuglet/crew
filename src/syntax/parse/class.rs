use crate::syntax::parse::macros::AstAttrQualifier;
use crate::syntax::parse::path::{AstPathTree, AstVisQualifier};
use crate::syntax::parse::ty::AstType;
use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_ident, util_match_kw, util_match_punct,
    util_match_specific_kw, util_punct_matcher, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenIdent, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, enum_meta, EnumMeta, VariantOf};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, StreamReader};

#[derive(Debug, Clone)]
pub struct AstClassItem {
    pub qualifiers: Vec<AstClassQualifier>,
    pub kind: AstClassItemKind,
}

impl AstClassItem {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match leading qualifiers
            let qualifiers = reader.consume_while(AstClassQualifier::parse).collect();

            // Match actual item
            let kind = AstClassItemKind::parse(reader)?;

            Some(Self { qualifiers, kind })
        })
    }

    pub fn parse_group_inner(reader: &mut TokenStreamReader) -> Option<Vec<Self>> {
        let parts = reader.consume_while(Self::parse).collect();

        if !reader.consume().is_none() {
            return None;
        }

        Some(parts)
    }
}

// === Item kinds === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstClassItemKind {
        Field(AstClassItemField),
        Block(AstClassItemBlock),
        Remote(AstClassItemRemote),
    }
}

impl AstClassItemKind {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstClassItemField::parse(reader)?.wrap()),
            |reader| Some(AstClassItemBlock::parse(reader)?.wrap()),
            |reader| Some(AstClassItemRemote::parse(reader)?.wrap()),
        )
    }
}

enum_meta! {
    #[derive(Debug)]
    pub enum(AstKeyword) AstFieldType {
        Val = AstKeyword::Val,
        Var = AstKeyword::Var,
    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemField {
    pub prefix: AstFieldType,
    pub name: String,
    pub ty: AstType,
}

impl AstClassItemField {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match prefix
            let prefix = util_match_kw(reader)
                .and_then(|kw| AstFieldType::find_where(|_, field_kw| *field_kw == kw))?;

            // Match field name
            let name = util_match_ident(reader)?;

            // Match type annotation
            let _ = util_match_punct(reader, PunctChar::Colon, None)?;
            let ty = AstType::parse(reader)?;

            // Match semicolon
            let _ = util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self {
                prefix,
                name: name.take_text(),
                ty,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemBlock {
    pub items: Vec<AstClassItem>,
}

impl AstClassItemBlock {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            let items = AstClassItem::parse_group_inner(&mut group.reader())?;
            Some(Self { items })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemRemote {
    pub name: String,
    pub rename_as: Option<String>,
}

impl AstClassItemRemote {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match prefix
            if !util_match_specific_kw(reader, AstKeyword::On) {
                return None;
            }

            // Match field name
            let name = util_match_ident(reader)?;

            // Match optional rename (significant in impl-hole blocks)
            let rename_as = reader.lookahead(|reader| {
                if !util_match_specific_kw(reader, AstKeyword::As) {
                    return None;
                }

                util_match_ident(reader)
            });

            // Match semicolon
            let _ = util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self {
                name: name.take_text(),
                rename_as: rename_as.map(TokenIdent::take_text),
            })
        })
    }
}

// === Qualifiers === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstClassQualifier {
        Attr(AstAttrQualifier),
        In(AstClassQualifierIn),
        Out(AstClassQualifierOut),
        Pub(AstVisQualifier),
        Impl(AstClassQualifierImpl),
        Static(AstClassQualifierStatic),
    }
}

impl AstClassQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstAttrQualifier::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierIn::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierOut::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierStatic::parse(reader)?.wrap()),
            |reader| Some(AstClassQualifierImpl::parse(reader)?.wrap()),
            |reader| Some(AstVisQualifier::parse(reader)?.wrap()),
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
pub struct AstClassQualifierImpl {
    path: AstImplPath,
}

impl AstClassQualifierImpl {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            if !util_match_specific_kw(reader, AstKeyword::Impl) {
                return None;
            }

            let group = util_match_group_delimited(reader, GroupDelimiter::Paren)?;

            let mut inner_reader = group.reader();
            let path = AstImplPath::parse(&mut inner_reader);
            if !inner_reader.consume().is_none() {
                return None;
            }

            Some(Self { path })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstImplPath {
    pub parts: Vec<String>,
    pub hole_suffix: bool,
}

impl AstImplPath {
    pub fn parse(reader: &mut TokenStreamReader) -> Self {
        let matcher = util_punct_matcher(PunctChar::Period);
        let mut delimited = DelimiterMatcher::new_start(&matcher);

        let parts = reader
            .consume_while(|reader| {
                // TODO: Make multiple lookahead more ergonomic.
                delimited.lookahead(|delimited| {
                    let _ = delimited.next(reader)?;
                    Some(util_match_ident(reader)?.take_text())
                })
            })
            .collect();

        let hole_suffix = reader
            .lookahead(|reader| {
                let _ = delimited.next(reader)?;

                if !util_match_specific_kw(reader, AstKeyword::Hole) {
                    return None;
                }

                Some(())
            })
            .is_some();

        Self { parts, hole_suffix }
    }
}

#[derive(Debug, Clone)]
pub struct AstClassQualifierStatic;

impl AstClassQualifierStatic {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        util_match_specific_kw(reader, AstKeyword::Static).then_some(Self)
    }
}
