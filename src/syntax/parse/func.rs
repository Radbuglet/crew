use crate::syntax::parse::expr::AstScope;
use crate::syntax::parse::ty::AstType;
use crate::syntax::parse::util::{
    util_match_eof, util_match_func_arrow, util_match_group_delimited, util_match_ident,
    util_match_kw, util_match_punct, util_match_specific_kw, util_punct_matcher, AstKeyword,
};
use crate::syntax::token::{GroupDelimiter, PunctChar, TokenStreamReader};
use crate::util::enum_utils::{enum_meta, EnumMeta};
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader};

#[derive(Debug, Clone)]
pub struct AstFuncDef {
    pub name: String,
    pub generics: Option<AstFuncGenerics>,
    pub args: Vec<(String, AstType)>,
    pub expr: Option<AstScope>,
    pub ret: Option<AstType>,
}

impl AstFuncDef {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_specific_kw(reader, AstKeyword::Fn)?;

            // Match name
            let name = util_match_ident(reader)?;

            // Match optional generics
            let generics = AstFuncGenerics::parse(reader)?;

            // Match arguments
            let args = {
                let group = util_match_group_delimited(reader, GroupDelimiter::Paren)?;
                let mut reader = group.reader();

                // Collect args
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                let args = reader
                    .consume_while(|reader| {
                        delimited.next(reader)?;

                        // Match name
                        let name = util_match_ident(reader)?;

                        // Match type annotation
                        util_match_punct(reader, PunctChar::Colon, None)?;
                        let ty = AstType::parse(reader)?;

                        Some((name.text(), ty))
                    })
                    .collect();

                // Expect EOF
                util_match_eof(&mut reader)?;

                args
            };

            // Match optional return type
            let ret = if util_match_func_arrow(reader).is_some() {
                Some(AstType::parse(reader)?)
            } else {
                None
            };

            // Match input expression
            let expr = match_choice!(
                reader,
                // Match concrete
                |reader| Some(Some(AstScope::parse_braced(reader)?)),
                // Match prototype
                |reader| {
                    util_match_punct(reader, PunctChar::Semicolon, None)?;
                    Some(None)
                }
            )?;

            Some(Self {
                name: name.text(),
                generics,
                args,
                ret,
                expr,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstFuncGenerics {
    pub params: Vec<(String, Option<AstType>)>,
}

impl AstFuncGenerics {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Option<Self>> {
        reader.lookahead(|reader| {
            // Match '<'
            if util_match_punct(reader, PunctChar::Less, None).is_some() {
                let mut delimited =
                    DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

                // Match parameters
                let params = reader
                    .consume_while(|reader| {
                        delimited.next(reader)?;

                        let name = util_match_ident(reader)?;
                        let ty = if util_match_punct(reader, PunctChar::Colon, None).is_some() {
                            Some(AstType::parse(reader)?)
                        } else {
                            None
                        };

                        Some((name.text(), ty))
                    })
                    .collect();

                // Match '>'
                util_match_punct(reader, PunctChar::Greater, None)?;

                Some(Some(Self { params }))
            } else {
                Some(None)
            }
        })
    }
}

enum_meta! {
    #[derive(Debug)]
    pub enum(AstKeyword) AstFieldType {
        Val = AstKeyword::Val,
        Var = AstKeyword::Var,
    }
}

impl AstFieldType {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_kw(reader)
                .and_then(|kw| AstFieldType::find_where(|_, field_kw| *field_kw == kw))
        })
    }
}
