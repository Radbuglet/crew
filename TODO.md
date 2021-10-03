# To-Do

## Tokenizer

- [x] Implement public tokenizer IR interface
- [x] Implement token/span printing
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
- [ ] Implement error recovery system
- [ ] Test suite

## Infrastructure

- [x] Trace logging
- [ ] Implement page-based interner
- [ ] Generic copy-on-write system
- [ ] Diagnostic system
- [ ] Better backing system
- [ ] Task system
- [ ] Crash reporter

## Parser

- [ ] Create reader
- [ ] Create parsing traits (similar to `syn`)
- [ ] Define AST

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
