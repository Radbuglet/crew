use crate::syntax::parse::util::{
    util_match_group_delimited, util_match_ident, util_match_ident_or_kw, util_match_punct,
    util_match_specific_kw, util_match_turbo, util_punct_matcher, AstKeyword, IdentOrKw,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, RepFlow, StreamReader};

// === Shared === //

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum AstPathRoot {
    Unspecified,
    Absolute,
    Self_,
    Crate,
}

impl AstPathRoot {
    pub fn parse(reader: &mut TokenStreamReader) -> Self {
        match_choice!(
            reader,
            // Absolute
            |reader| if util_match_turbo(reader).is_some() {
                Some(Self::Absolute)
            } else {
                None
            },
            // Self
            |reader| if util_match_specific_kw(reader, AstKeyword::Self_) {
                Some(Self::Self_)
            } else {
                None
            },
            // Crate
            |reader| if util_match_specific_kw(reader, AstKeyword::Crate) {
                Some(Self::Crate)
            } else {
                None
            },
        )
        .unwrap_or(Self::Unspecified)
    }

    pub fn expects_turbo(self) -> bool {
        match self {
            Self::Unspecified => false,
            Self::Absolute => false,
            Self::Self_ => true,
            Self::Crate => true,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstPathPart {
    Super(usize),
    Lit(String),
}

impl AstPathPart {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            // Match super literal
            |reader| if util_match_specific_kw(reader, AstKeyword::Super) {
                Some(Self::Super(1))
            } else {
                None
            },
            // Match super carets
            |reader| {
                let caret_count = reader.consume_while(|reader| {
                    util_match_punct(reader, PunctChar::Caret, None).is_some()
                });

                if caret_count > 0 {
                    Some(Self::Super(caret_count))
                } else {
                    None
                }
            },
            // Match literal
            |reader| match util_match_ident_or_kw(reader) {
                Some(IdentOrKw { raw, kw: None }) => Some(Self::Lit(raw.text.clone())),
                _ => None,
            }
        )
    }

    pub fn parse_list(reader: &mut TokenStreamReader, expect_leading_turbo: bool) -> Vec<Self> {
        let mut parts = Vec::new();
        let mut delimited = DelimiterMatcher::new(util_match_turbo, expect_leading_turbo);

        reader.consume_while(|reader| {
            // Expect delimiter
            if delimited.next(reader).is_none() {
                return false;
            }

            // Push path part
            let part = match Self::parse(reader) {
                Some(part) => part,
                None => return false,
            };

            parts.push(part);
            true
        });

        parts
    }
}

// === Expr paths === //

#[derive(Debug, Clone)]
pub struct AstDirectPath {
    pub root: AstPathRoot,
    pub parts: Vec<AstPathPart>,
}

impl AstDirectPath {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match root
            let root = AstPathRoot::parse(reader);

            // Match subsequent parts
            let parts = AstPathPart::parse_list(reader, root.expects_turbo());

            // Determine whether this is an actual pattern match
            if parts.is_empty() {
                match root {
                    // Nothing in this pattern matched.
                    AstPathRoot::Unspecified => return None,
                    // This is just a lone "::".
                    AstPathRoot::Absolute => return None,

                    // These two can stand on their own
                    AstPathRoot::Self_ => {}
                    AstPathRoot::Crate => {}
                }
            }

            Some(Self { root, parts })
        })
    }
}

// === Mod paths === //

#[derive(Debug, Clone)]
pub struct AstPathTree {
    pub root: AstPathRoot,
    pub node: AstPathNode,
}

impl AstPathTree {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let root = AstPathRoot::parse(reader);
            let node = AstPathNode::parse(reader, root.expects_turbo())?;

            Some(Self { root, node })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstPathNode {
    pub parts: Vec<AstPathPart>,
    pub terminator: AstPathTerminator,
}

impl AstPathNode {
    pub fn parse(reader: &mut TokenStreamReader, expects_leading_turbo: bool) -> Option<Self> {
        let mut parts = Vec::new();
        let mut terminator = AstPathTerminator::Finish;
        let mut delimited = DelimiterMatcher::new(util_match_turbo, expects_leading_turbo);

        reader.consume_while(|reader| {
            // Match delimiter
            if !delimited.next(reader).is_some() {
                return RepFlow::Reject;
            }

            // Match either a terminator or a path part
            enum Matched {
                Terminator(AstPathTerminator),
                Part(AstPathPart),
            }

            match match_choice!(
                reader,
                |reader| Some(Matched::Terminator(AstPathTerminator::parse(reader)?)),
                |reader| Some(Matched::Part(AstPathPart::parse(reader)?)),
            ) {
                Some(Matched::Terminator(matched)) => {
                    terminator = matched;
                    RepFlow::Finish
                }
                Some(Matched::Part(part)) => {
                    parts.push(part);
                    RepFlow::Continue
                }
                None => RepFlow::Reject,
            }
        });

        Some(Self { parts, terminator })
    }
}

#[derive(Debug, Clone)]
pub enum AstPathTerminator {
    Finish,
    Rename { target: String, rename: String },
    Tree(Vec<AstPathNode>),
    Wildcard,
}

impl AstPathTerminator {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            // Rename
            |reader| {
                let real_id = util_match_ident(reader)?;

                if !util_match_specific_kw(reader, AstKeyword::As) {
                    return None;
                }

                let target_id = util_match_ident(reader)?;

                Some(Self::Rename {
                    target: real_id.text.clone(),
                    rename: target_id.text.clone(),
                })
            },
            // Tree
            |reader| if let Some(group) = util_match_group_delimited(reader, GroupDelimiter::Brace)
            {
                let mut reader = group.stream.reader();
                let mut nodes = Vec::new();

                // Match interior list
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                while let Some(_) = delimited.next(&mut reader) {
                    // The node is optional (e.g. for trailing commas)
                    if let Some(node) = AstPathNode::parse(&mut reader, false) {
                        nodes.push(node);
                    }
                }

                // Match inner group EOF
                if reader.consume().is_some() {
                    return None;
                }

                Some(Self::Tree(nodes))
            } else {
                None
            },
            // Wildcard
            |reader| if util_match_punct(reader, PunctChar::Asterisk, None).is_some() {
                Some(Self::Wildcard)
            } else {
                None
            }
        )
    }
}