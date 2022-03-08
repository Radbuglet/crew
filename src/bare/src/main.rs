#![allow(dead_code)]
#![feature(allocator_api)]
#![feature(bool_to_option)]
#![feature(build_hasher_simple_hash_one)]
#![feature(coerce_unsized)]
#![feature(decl_macro)]
#![feature(int_log)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]
#![feature(never_type)]
#![feature(str_internals)]
#![feature(try_trait_v2)]
#![feature(unsize)]

pub mod semantic;
pub mod syntax;
pub mod util;

fn main() -> Result<(), ()> {
    use crate::syntax::ast::module::AstModule;
    use crate::syntax::ast::util::AstCxBundle;
    use crate::syntax::diagnostic::Diagnostics;
    use crate::syntax::span::SourceFile;
    use crate::syntax::token::tokenize::tokenize_file;
    use std::path::PathBuf;

    // Initialize logger
    env_logger::init();

    // Tokenize source code
    let tokens = {
        let source = SourceFile::from_file(PathBuf::from("examples/syn/mod_item.crew")).unwrap();
        let mut diag = Diagnostics::new();
        let tokens = tokenize_file(&mut source.reader(), &mut diag);
        diag.temp_display();
        tokens?
    };

    println!("Tokenized source: {}", tokens);

    // Parse AST
    let parsed = {
        let mut cx = AstCxBundle {
            diag: Diagnostics::new(),
        };
        let parsed = AstModule::parse((&mut cx, &mut tokens.reader()));
        cx.diag.temp_display();
        parsed.ok_or(())?
    };

    println!("Parsed source: {:#?}", parsed);
    Ok(())
}
