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
#![feature(unsize)]

pub mod syntax;
pub mod util;

fn main() -> Result<(), ()> {
    use crate::syntax::diagnostic::Diagnostics;
    use crate::syntax::parse::module::AstModule;
    use crate::syntax::span::SourceFile;
    use crate::syntax::token::tokenize_file;
    use std::path::PathBuf;

    // Initialize logger
    env_logger::init();

    // Tokenize source code
    let tokens = {
        let source = SourceFile::from_file(PathBuf::from("examples/syn/class.crew")).unwrap();
        let mut diag = Diagnostics::new();
        let tokens = tokenize_file(&mut source.reader(), &mut diag);
        diag.temp_display();
        tokens?
    };

    println!("Tokenized source: {}", tokens);

    // Parse AST
    let parsed = AstModule::parse(&mut tokens.reader());
    println!("Parsed source: {:#?}", parsed);
    Ok(())
}
