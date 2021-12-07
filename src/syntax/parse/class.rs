use crate::syntax::parse::func::{AstFieldType, AstFuncDef};
use crate::syntax::parse::macros::AstAttrQualifier;
use crate::syntax::parse::path::{AstPathTree, AstVisQualifier};
use crate::syntax::parse::ty::AstType;
use crate::syntax::parse::util::{
    util_match_eof, util_match_group_delimited, util_match_ident, util_match_punct,
    util_match_specific_kw, util_punct_matcher, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenIdent, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader};

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
        util_match_eof(reader)?;
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
        Type(AstClassItemType),
        Func(AstFuncDef),
    }
}

impl AstClassItemKind {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstClassItemField::parse(reader)?.wrap()),
            |reader| Some(AstClassItemBlock::parse(reader)?.wrap()),
            |reader| Some(AstClassItemRemote::parse(reader)?.wrap()),
            |reader| Some(AstClassItemType::parse(reader)?.wrap()),
            |reader| Some(AstFuncDef::parse(reader)?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemField {
    pub prefix: AstFieldType,
    pub name: String,
    pub ty: AstType,
    pub impl_block: Option<Vec<AstClassItem>>,
}

impl AstClassItemField {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match prefix
            let prefix = AstFieldType::parse(reader)?;

            // Match field name
            let name = util_match_ident(reader)?;

            // Match type annotation
            util_match_punct(reader, PunctChar::Colon, None)?;
            let ty = AstType::parse(reader)?;

            // Match optional impl block
            let impl_block = reader.lookahead(|reader| {
                if util_match_specific_kw(reader, AstKeyword::Impl).is_some() {
                    let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
                    Some(Some(AstClassItem::parse_group_inner(&mut group.reader())?))
                } else {
                    Some(None)
                }
            })?;

            // Match semicolon
            if impl_block.is_none() {
                util_match_punct(reader, PunctChar::Semicolon, None)?;
            }

            Some(Self {
                prefix,
                name: name.text(),
                ty,
                impl_block,
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
            util_match_specific_kw(reader, AstKeyword::On)?;

            // Match field name
            let name = util_match_ident(reader)?;

            // Match optional rename (significant in impl-hole blocks)
            let rename_as = reader.lookahead(|reader| {
                util_match_specific_kw(reader, AstKeyword::As)?;
                util_match_ident(reader)
            });

            // Match semicolon
            util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self {
                name: name.text(),
                rename_as: rename_as.map(TokenIdent::text),
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstClassItemType {
    pub name: String,
    pub annotation: Option<AstType>,
    pub equals: Option<AstType>,
}

impl AstClassItemType {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_specific_kw(reader, AstKeyword::Type)?;
            let name = util_match_ident(reader)?;

            let annotation = reader.lookahead(|reader| {
                util_match_punct(reader, PunctChar::Colon, None)?;
                AstType::parse(reader)
            });

            let equals = reader.lookahead(|reader| {
                util_match_punct(reader, PunctChar::Equals, None)?;
                AstType::parse(reader)
            });

            util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self {
                name: name.text(),
                annotation,
                equals,
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
            util_match_specific_kw(reader, AstKeyword::In)?;
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
            util_match_specific_kw(reader, AstKeyword::Out)?;

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
                util_match_eof(&mut reader)?;
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
            |reader| util_match_specific_kw(reader, AstKeyword::Hole).and(Some(Self::Infer)),
            // Match dynamic
            |reader| util_match_specific_kw(reader, AstKeyword::Dynamic).and(Some(Self::Dynamic))
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
            util_match_specific_kw(reader, AstKeyword::Impl)?;

            let group = util_match_group_delimited(reader, GroupDelimiter::Paren)?;

            let mut reader = group.reader();
            let path = AstImplPath::parse(&mut reader);
            util_match_eof(&mut reader)?;

            Some(Self { path })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstImplPath {
    pub parts: Vec<String>,
    pub has_hole: bool,
}

impl AstImplPath {
    pub fn parse(reader: &mut TokenStreamReader) -> Self {
        let matcher = util_punct_matcher(PunctChar::Period);
        let mut delimited = DelimiterMatcher::new_start(&matcher);

        let parts = reader
            .consume_while(|reader| {
                // TODO: Make multiple lookahead more ergonomic.
                delimited.lookahead(|delimited| {
                    delimited.next(reader)?;
                    Some(util_match_ident(reader)?.text())
                })
            })
            .collect();

        let has_hole = reader
            .lookahead(|reader| {
                delimited.next(reader)?;
                util_match_specific_kw(reader, AstKeyword::Hole)
            })
            .is_some();

        Self { parts, has_hole }
    }
}

#[derive(Debug, Clone)]
pub struct AstClassQualifierStatic;

impl AstClassQualifierStatic {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        util_match_specific_kw(reader, AstKeyword::Static).and(Some(Self))
    }
}
