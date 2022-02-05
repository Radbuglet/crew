//! ## Stage 3.1 - Identifier Linking
//!
//! This pass transforms the raw AST tree into a graph of modules whose items are properly inter-linked.
//! During this pass, the following program semantics are checked:
//!
//! - Visibility
//! - Re-export visibility
//! - Name conflicts and shadowing
//! - Extension method availability
//!
//! After the linking pass has completed, macros are expanded and new syntax is parsed and linked.
//! Once this is done, the program can begin type checking in stage 3.2.
