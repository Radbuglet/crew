# To-Do

## Tokenizer

- [x] Implement public tokenizer IR interface
- [ ] Implement token/span debug printer
- [ ] Improve string literal handling:
  - [x] Multiline strings 
  - [ ] Unicode escapes
  - [ ] Single characters
  - [ ] Multiline indent bars
  - [ ] Custom delimiters
- [ ] Finish numeric literal parsing (how do we handle property accesses and ranges)
- [ ] Preserve doc and delimiter comments
- [ ] Revisit handling of invalid newlines/characters
- [ ] Reconsider character groups
- [ ] Test suite

## Infrastructure

- [x] Trace logging
- [ ] Diagnostic system
- [ ] Better backing system
- [ ] More generic interner (`bumpalo`-style allocator with pages?)
- [ ] Task system
- [ ] Crash reporter

## Parser

- [ ] Create reader
- [ ] Create parsing traits (similar to `syn`)
- [ ] Define expressions
- [ ] Define attributes
- [ ] Define items
- [ ] Define modules

## Semantic

- [ ] Module traversal
- [ ] Identifier linking
- [ ] Visibility pass
- [ ] Dead code warnings (if necessary)
- [ ] Macro expansion (token-level macros pass)
- [ ] Type check/inference & provider validity checks
- [ ] Macro expansion (meta-level macros pass)
- [ ] Closure analysis (capture soundness, cross-closure jump validity and expansion)\
- [ ] Match exhaustiveness validation
- [ ] Control flow analysis
- [ ] Bytecode generation from control flow graph

## Interpreter

- [ ] In-memory representation
- [ ] (de)Serialization
- [ ] Object representation
- [ ] Garbage collector
- [ ] Interpreter

# Feature Wishlist

- [x] Multiline strings with templating
- [x] Nested comments
- [ ] `//>` region comments
- [ ] Doc comments
- [ ] `out` and `ref` parameters
- [ ] Unified enum and type matching system through `is`
- [ ] Linking `in` at runtime if they are statically unbound (should allow late binding with checks)
- [ ] Allow users to specify the scope of who can specify inputs.
- [ ] `in out` linking inference
- [ ] Try unwrap (`?`) syntax
- [ ] `return` to named block
- [ ] `break`/`continue`/`return` within specific closures that support it (done by propagating a `ControlFlow<T, L>` result object)
- [ ] Explicit inlining by qualifying variables with `~`
- [ ] Combine Java-style enum metadata with Rust-style sum type enums
- [ ] Actual `statics`
- [ ] Programmatic `TokenStream` macros (support macros by example?).
- [ ] Attribute macros which can operate on a typed IR
- [ ] Apply qualifiers to blocks, allow qualifiers to span several lines
- [ ] Type aliases qualified with `in` as an alternative to generics.
- [ ] Remote qualifiers using the `<qualifier> on <item>` syntax. Mostly used for `impl`.
- [ ] Class/tuple deconstruction
- [ ] Operator overloading (differentiate between assign and copy)
- [ ] Reflection
- [ ] Class properties
- [ ] Programmatic dynamic exposition (`IDynamicObject` interface)
- [ ] Allow users to dynamically specify input fields (as either values or property getters) and methods (as closures).
- [ ] Unified `async` and iterator handling through coroutines.
- [ ] No required runtime, allow host to decide which function is the entry point.
