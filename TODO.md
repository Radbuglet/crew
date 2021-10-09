# To-Do

## Infrastructure

- [x] Trace logging
- [ ] Efficient storage mechanisms:
  - [x] ~~Bump allocator~~ (`bumaplo` is sufficiently powerful when using the allocator API)
  - [ ] Fork allocation primitives
  - [ ] Interner
- [ ] Diagnostic system
- [ ] Better backing system
- [ ] Task system
- [ ] Crash reporter

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
- [ ] Preserve insignificant characters (e.g. whitespace, comments) for formatting.
- [ ] Revisit handling of invalid newlines/characters
- [ ] Reconsider character groups
- [ ] Implement error recovery system
- [ ] Test suite

## Parser

- [x] Create reader
- [ ] Create in-place array folding mechanisms
- [ ] Define expression AST
- [ ] Implement expression parsing
- [ ] Define scopes and their parsing
- [ ] Define modules and their parsing
- [ ] Implement diagnostic and error recovery system
- [ ] Test suite

# Feature Wishlist

- [x] Multiline strings with templating
- [x] Nested comments
- [ ] `//>` region comments
- [ ] Doc comments
- [ ] Unified enum and type matching system through `is`
- [ ] Linking `in` at runtime if they are statically unbound (should allow late binding with checks)
- [ ] Allow users to specify the scope of who can provide values for a given `in`.
- [ ] `in out` linking inference
- [ ] Error propagation (`?`) syntax with implicit conversion
- [ ] `return` to named block
- [ ] `break`/`continue`/`return` within specific closures that support it (done by propagating a `ControlFlow<T, L>` result object)
- [ ] Make garbage collected pointer types (ideally handled in userland with GC alloc intrinsics), implement move/clone semantics for owned values.
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
