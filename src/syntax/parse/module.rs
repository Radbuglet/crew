use crate::syntax::parse::class::AstClassItem;
use crate::syntax::parse::macros::{AstAnyAttrMacro, AstAttrQualifier};
use crate::syntax::parse::path::{AstPathTree, AstVisQualifier};
use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_ident, util_match_punct, util_match_specific_kw,
    AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, LookaheadReader, StreamReader};

#[derive(Debug, Clone)]
pub struct AstModule {
    pub inner_attrs: Vec<AstAnyAttrMacro>,
    pub parts: Vec<AstModItem>,
}

impl AstModule {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        // Match inner attributes
        let inner_attrs = reader.consume_while(AstAnyAttrMacro::parse_inner).collect();

        // Match parts
        let parts = Self::parse_parts(reader)?;

        Some(Self { inner_attrs, parts })
    }

    pub fn parse_parts(reader: &mut TokenStreamReader) -> Option<Vec<AstModItem>> {
        reader.lookahead(|reader| {
            // Collect parts
            let parts = reader
                .consume_while(|reader| {
                    // Collect qualifiers
                    let qualifiers = reader.consume_while(AstModQualifier::parse).collect();

                    // Collect item
                    let kind = AstModItemKind::parse(reader)?;

                    Some(AstModItem { qualifiers, kind })
                })
                .collect();

            // Expect EOF
            if !reader.consume().is_none() {
                return None;
            }

            Some(parts)
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModItem {
    pub qualifiers: Vec<AstModQualifier>,
    pub kind: AstModItemKind,
}

// === Item kinds === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstModItemKind {
        Mod(AstModModule),
        Block(AstModBlock),
        Use(AstModUse),
        Class(AstModClass),
    }
}

impl AstModItemKind {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstModModule::parse(reader)?.wrap()),
            |reader| Some(AstModBlock::parse(reader)?.wrap()),
            |reader| Some(AstModUse::parse(reader)?.wrap()),
            |reader| Some(AstModClass::parse(reader)?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstModModule {
    pub name: String,
    pub inline: Option<AstModule>,
}

impl AstModModule {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match mod keyword
            util_match_specific_kw(reader, AstKeyword::Mod)?;

            // Match module name
            let name = util_match_ident(reader)?;

            // Match inline contents
            let inline = match util_match_group_delimited(reader, GroupDelimiter::Brace) {
                Some(brace) => Some(AstModule::parse(&mut brace.reader())?),
                None => None,
            };

            // Otherwise, match the semicolon
            if inline.is_none() {
                util_match_punct(reader, PunctChar::Semicolon, None)?;
            }

            Some(Self {
                name: name.text(),
                inline,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModBlock {
    pub parts: Vec<AstModItem>,
}

impl AstModBlock {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            let parts = AstModule::parse_parts(&mut group.reader())?;

            Some(Self { parts })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModUse {
    pub path: AstPathTree,
}

impl AstModUse {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match "use"
            util_match_specific_kw(reader, AstKeyword::Use)?;

            // Match tree
            let path = AstPathTree::parse(reader)?;

            // Match semicolon
            util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self { path })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModClass {
    name: String,
    items: Vec<AstClassItem>,
}

impl AstModClass {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_specific_kw(reader, AstKeyword::Class)?;
            let name = util_match_ident(reader)?;
            let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            let items = AstClassItem::parse_group_inner(&mut group.reader())?;

            Some(Self {
                name: name.text(),
                items,
            })
        })
    }
}

// === Qualifiers === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstModQualifier {
        Vis(AstVisQualifier),
        Attr(AstAttrQualifier),
    }
}

impl AstModQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstVisQualifier::parse(reader)?.wrap()),
            |reader| Some(AstAttrQualifier::parse(reader)?.wrap())
        )
    }
}
