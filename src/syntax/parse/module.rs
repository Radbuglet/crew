use crate::syntax::parse::macros::AstAnyAttrMacro;
use crate::syntax::parse::path::AstPathTree;
use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_ident, util_match_punct, util_match_specific_kw,
    util_punct_matcher, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::enum_utils::{enum_categories, VariantOf};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, StreamReader};

// === Modules === //

#[derive(Debug, Clone)]
pub struct AstModule {
    pub inner_attrs: Vec<AstAnyAttrMacro>,
    pub parts: Vec<AstModItem>,
}

impl AstModule {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        // Match inner attributes
        let mut inner_attrs = Vec::new();

        reader.consume_while(|reader| match AstAnyAttrMacro::parse(reader) {
            Some((true, attr)) => {
                inner_attrs.push(attr);
                true
            }
            _ => false,
        });

        // Match parts
        let parts = Self::parse_parts(reader)?;

        Some(Self { inner_attrs, parts })
    }

    pub fn parse_parts(reader: &mut TokenStreamReader) -> Option<Vec<AstModItem>> {
        reader.lookahead(|reader| {
            let mut parts = Vec::new();

            reader.consume_while(|reader| {
                let mut qualifiers = Vec::new();

                // Collect qualifiers
                reader.consume_while(|reader| {
                    if let Some(qualifier) = AstModQualifier::parse(reader) {
                        qualifiers.push(qualifier);
                        true
                    } else {
                        false
                    }
                });

                // Collect item
                let kind = match AstModItemKind::parse(reader) {
                    Some(kind) => kind,
                    None => return false,
                };

                parts.push(AstModItem { qualifiers, kind });

                true
            });

            // Expect EOF
            if !reader.peek().is_none() {
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

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstModItemKind {
        Mod(AstModModule),
        Block(AstModBlock),
    }
}

impl AstModItemKind {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstModModule::parse(reader)?.wrap()),
            |reader| Some(AstModBlock::parse(reader)?.wrap())
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
            if !util_match_specific_kw(reader, AstKeyword::Mod) {
                return None;
            }

            // Match module name
            let name = util_match_ident(reader)?;

            // Match inline contents
            let inline = match util_match_group_delimited(reader, GroupDelimiter::Brace) {
                Some(brace) => Some(AstModule::parse(&mut brace.stream.reader())?),
                None => None,
            };

            // Otherwise, match the semicolon
            if inline.is_none() {
                let _ = util_match_punct(reader, PunctChar::Semicolon, None)?;
            }

            Some(Self {
                name: name.text.clone(),
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
            let parts = AstModule::parse_parts(&mut group.stream.reader())?;

            Some(Self { parts })
        })
    }
}

// === Qualifiers === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstModQualifier {
        Vis(AstModVisQualifier),
        Attr(AstModAttrQualifier),
    }
}

impl AstModQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            |reader| Some(AstModVisQualifier::parse(reader)?.wrap()),
            |reader| Some(AstModAttrQualifier::parse(reader)?.wrap())
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstModVisQualifier {
    pub visible_to: Vec<AstPathTree>,
}

impl AstModVisQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match "pub"
            if !util_match_specific_kw(reader, AstKeyword::Pub) {
                return None;
            }

            // Match optional path list
            let mut visible_to = Vec::new();
            if let Some(paren) = util_match_group_delimited(reader, GroupDelimiter::Paren) {
                // TODO: We should probably add a generic list parser.
                let mut reader = paren.stream.reader();

                // Match interior list
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                while let Some(_) = delimited.next(&mut reader) {
                    // The node is optional (e.g. for trailing commas)
                    if let Some(tree) = AstPathTree::parse(&mut reader) {
                        visible_to.push(tree);
                    }
                }

                // Match inner group EOF
                if reader.consume().is_some() {
                    return None;
                }
            }

            // Construct
            Some(Self { visible_to })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstModAttrQualifier {
    pub attr: AstAnyAttrMacro,
}

impl AstModAttrQualifier {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| match AstAnyAttrMacro::parse(reader) {
            Some((false, attr)) => Some(Self { attr }),
            _ => None,
        })
    }
}
