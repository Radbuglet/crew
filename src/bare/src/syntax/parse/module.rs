use crate::syntax::parse::macros::{AstAnyAttrMacro, AstAttrQualifier};
use crate::syntax::parse::path::{AstPathTree, AstVisQualifier};
use crate::syntax::parse::util::{
    util_match_eof, util_match_group_delimited, util_match_ident, util_match_punct,
    util_match_specific_kw, AstCx, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, LookaheadReader};

#[derive(Debug, Clone)]
pub struct AstModule {
    pub inner_attrs: Vec<AstAnyAttrMacro>,
    pub items: Vec<AstModItem>,
}

impl AstModule {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        // Match inner attributes
        let inner_attrs = reader
            .consume_while(|reader| AstAnyAttrMacro::parse_inner((cx, reader)))
            .collect();

        // Match parts
        let parts = Self::parse_parts((cx, reader))?;

        Some(Self {
            inner_attrs,
            items: parts,
        })
    }

    pub fn parse_parts((cx, reader): AstCx) -> Option<Vec<AstModItem>> {
        reader.lookahead(|reader| {
            // Collect parts
            let parts = reader
                .consume_while(|reader| AstModItem::parse((cx, reader)))
                .collect();

            // Expect EOF
            util_match_eof(reader)?;

            Some(parts)
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModItem {
    pub qualifiers: Vec<AstModQualifier>,
    pub kind: AstModItemKind,
}

impl AstModItem {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        // Collect qualifiers
        let qualifiers = reader
            .consume_while(|reader| AstModQualifier::parse((cx, reader)))
            .collect();

        // Collect item
        let kind = AstModItemKind::parse((cx, reader))?;

        Some(AstModItem { qualifiers, kind })
    }
}

// === Item kinds === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstModItemKind {
        Mod(AstModModule),
        Block(AstModBlock),
        Use(AstModUse),
    }
}

impl AstModItemKind {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstModModule::parse((cx, reader))?.wrap()),
            |reader| Some(AstModBlock::parse((cx, reader))?.wrap()),
            |reader| Some(AstModUse::parse((cx, reader))?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstModModule {
    pub name: String,
    pub inline: Option<AstModule>,
}

impl AstModModule {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match mod keyword
            util_match_specific_kw(reader, AstKeyword::Mod)?;

            // Match module name
            let name = util_match_ident(reader)?;

            // Match inline contents
            let inline = match util_match_group_delimited(reader, GroupDelimiter::Brace) {
                Some(brace) => Some(AstModule::parse((cx, &mut brace.reader()))?),
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
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            let parts = AstModule::parse_parts((cx, &mut group.reader()))?;

            Some(Self { parts })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModUse {
    pub path: AstPathTree,
}

impl AstModUse {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match "use"
            util_match_specific_kw(reader, AstKeyword::Use)?;

            // Match tree
            let path = AstPathTree::parse((cx, reader))?;

            // Match semicolon
            util_match_punct(reader, PunctChar::Semicolon, None)?;

            Some(Self { path })
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
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstVisQualifier::parse((cx, reader))?.wrap()),
            |reader| Some(AstAttrQualifier::parse((cx, reader))?.wrap())
        )
    }
}
