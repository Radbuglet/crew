use crate::syntax::ast::util::{
    util_match_eof, util_match_group_delimited, util_match_ident, util_match_ident_or_kw,
    util_match_punct, util_match_specific_kw, util_match_turbo, util_punct_matcher, AstCx,
    AstKeyword, IdentOrKw,
};
use crate::syntax::token::ir::{GroupDelimiter, PunctChar};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader, RepFlow};

// === Shared === //

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum AstPathRoot {
    Unspecified,
    Absolute,
    Self_,
    Crate,
}

impl AstPathRoot {
    pub fn parse((_cx, reader): AstCx) -> Self {
        match_choice!(
            reader,
            // Absolute
            |reader| util_match_turbo(reader).and(Some(Self::Absolute)),
            // Self
            |reader| util_match_specific_kw(reader, AstKeyword::Self_).and(Some(Self::Self_)),
            // Crate
            |reader| util_match_specific_kw(reader, AstKeyword::Crate).and(Some(Self::Crate)),
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
    pub fn parse((_cx, reader): AstCx) -> Option<Self> {
        match_choice!(
            reader,
            // Match super literal
            |reader| util_match_specific_kw(reader, AstKeyword::Super).and(Some(Self::Super(1))),
            // Match super carets
            |reader| {
                let caret_count = reader
                    .consume_while(|reader| {
                        util_match_punct(reader, PunctChar::Caret, None).is_some()
                    })
                    .count();

                if caret_count > 0 {
                    Some(Self::Super(caret_count))
                } else {
                    None
                }
            },
            // Match literal
            |reader| match util_match_ident_or_kw(reader) {
                Some(IdentOrKw { raw, kw: None }) => Some(Self::Lit(raw.text())),
                _ => None,
            },
        )
    }
}

// === Expr paths === //

#[derive(Debug, Clone)]
pub struct AstPathDirect {
    pub root: AstPathRoot,
    pub parts: Vec<AstPathPart>,
}

impl AstPathDirect {
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match root
            let root = AstPathRoot::parse((cx, reader));

            // Match subsequent parts
            let mut delimited = DelimiterMatcher::new(util_match_turbo, root.expects_turbo());
            let parts = reader
                .consume_while(|reader| {
                    // Expect delimiter
                    delimited.next(reader)?;

                    // Push path part
                    AstPathPart::parse((cx, reader))
                })
                .collect::<Vec<_>>();

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
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            let root = AstPathRoot::parse((cx, reader));
            let node = AstPathNode::parse((cx, reader), root.expects_turbo())?;

            Some(Self { root, node })
        })
    }

    pub fn parse_path_parens((cx, reader): AstCx) -> Option<Vec<AstPathTree>> {
        reader.lookahead(|reader| {
            if let Some(paren) = util_match_group_delimited(reader, GroupDelimiter::Paren) {
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                // Collect paths
                // TODO: Make this generic and add support for optional trailing commas
                let paths = paren
                    .reader()
                    .consume_while(|reader| {
                        delimited.next(reader)?;
                        AstPathTree::parse((cx, reader))
                    })
                    .collect();

                // Match inner group EOF
                util_match_eof(reader)?;

                Some(paths)
            } else {
                Some(Vec::new())
            }
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstPathNode {
    pub parts: Vec<AstPathPart>,
    pub terminator: AstPathTerminator,
}

impl AstPathNode {
    pub fn parse((cx, reader): AstCx, expects_leading_turbo: bool) -> Option<Self> {
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
                |reader| Some(Matched::Terminator(AstPathTerminator::parse((cx, reader))?)),
                |reader| Some(Matched::Part(AstPathPart::parse((cx, reader))?)),
            ) {
                Some(Matched::Terminator(matched)) => {
                    terminator = matched;
                    RepFlow::Finish(())
                }
                Some(Matched::Part(part)) => {
                    parts.push(part);
                    RepFlow::Continue(())
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
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        match_choice!(
            reader,
            // Rename
            |reader| {
                let real_id = util_match_ident(reader)?;
                util_match_specific_kw(reader, AstKeyword::As)?;
                let target_id = util_match_ident(reader)?;

                Some(Self::Rename {
                    target: real_id.text(),
                    rename: target_id.text(),
                })
            },
            // Tree
            |reader| if let Some(group) = util_match_group_delimited(reader, GroupDelimiter::Brace)
            {
                let mut reader = group.reader();
                let mut nodes = Vec::new();

                // Match interior list
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                while let Some(_) = delimited.next(&mut reader) {
                    // The node is optional (e.g. for trailing commas)
                    if let Some(node) = AstPathNode::parse((cx, &mut reader), false) {
                        nodes.push(node);
                    }
                }

                // Match inner group EOF
                util_match_eof(&mut reader)?;

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

// === Qualifiers === //

#[derive(Debug, Clone)]
pub struct AstVisQualifier {
    pub visible_to: Vec<AstPathTree>,
}

impl AstVisQualifier {
    //noinspection DuplicatedCode
    pub fn parse((cx, reader): AstCx) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match "pub"
            util_match_specific_kw(reader, AstKeyword::Pub)?;

            // Match optional path list
            let visible_to = AstPathTree::parse_path_parens((cx, reader))?;

            // Construct
            Some(Self { visible_to })
        })
    }
}
