use crate::syntax::parse::path::AstDirectPath;
use crate::syntax::parse::util::{util_match_group, util_match_punct};
use crate::syntax::token::{PunctChar, TokenGroup, TokenStreamReader};
use crate::util::reader::LookaheadReader;

#[derive(Debug, Clone)]
pub struct AstAnyAttrMacro {
    pub path: AstDirectPath,
    pub arg: Option<TokenGroup>,
}

impl AstAnyAttrMacro {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<(bool, Self)> {
        reader.lookahead(|reader| {
            // Match @ symbol
            let _ = util_match_punct(reader, PunctChar::At, None)?;

            // Match ! symbol
            let is_inner = util_match_punct(reader, PunctChar::Exclamation, None).is_some();

            // Match path to target attribute
            let path = AstDirectPath::parse(reader)?;

            // Match optional opaque token group passed directly to the macro.
            let arg = util_match_group(reader).map(Clone::clone);

            Some((is_inner, Self { path, arg }))
        })
    }
}
