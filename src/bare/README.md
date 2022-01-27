# Bare Crew

*The well-behaved meta-programming language.*

Bare Crew is a low-level C-style language which is primarily used to implement high-level languages such as Crew through the use of syntax-level macros.

Bare Crew programs are split up into modules. Each file is its own module and modules can include child modules through the use of inline `mod module_name { ... }` statements or file inclusion through its `mod file_name as module_name;` variant. Modules are composed of items.

In standard Bare Crew, the following are available as module items:

```
// Item imports from other modules.
use path::to::Item;
use path::to::{Several, Items};
use path::to::wildcarded::*;

// Modules
mod file_and_module_name;
mod file_name as new_module_name;
mod inline_module_name {
	// ...
}

// Type aliases
type AliasName = ...;

// Structures
// (unions may be achieved with the "@repr(union)" annotation)
struct StructName {
	field_1: FieldType,
	field_2: OtherFieldType,
}

struct StructName2(Type1, Type2);

struct UnitStruct;

// Discriminated unions (these may be more efficient than their manual union
// counterpart because of the way they abuse niche scalar layouts)
enum EnumName {
	variant UnitVariantName;
	variant StructVariantName {
		field: FieldType,
	};
	variant TupleVariantName(Type1, Type2);
}

// The uninhabited type is treated as a first-class citizen of this language.
enum Never {}

// Unsized types, however, must be defined by the language authors.

// Users can also use the standard tuple type:
type MyTupleType = (Type1, Type2);
type Unit = ();

// Functions
// The return type, when omitted, defaults to the unit tuple `()`.
fn functionName(param_1: Type1, param_2: Type2) -> ReturnType {
	<function body>
}

// Static variables
static STATIC_NAME: Type = <const initializer>;

// Constants
const CONST_NAME: Type = <const initializer>;

// Unparameterized boolean propositions.
// These are a special type of `const` that is computed solely through
// first-order logic. `prop`s can be converted into `const` bools but
// not the other way around.

// "prop" defines a new proposition which cannot be extended.
prop PropName = <prop>;

// "new prop" defines a new proposition which can be extended
// using "add prop" statements.
new prop PropName = <prop>;

// Users can specify which modules are allowed to add on to an
// existing proposition. If left empty, everyone can add to the
// proposition.
new(visible::to::{path1, path2}) prop PropName = <prop>;

// "add prop" defines an additional way in
// which a proposition can be proven. A
// proposition is true so long as at least
// one of the propositions is true.
add prop PropName = <prop>;

// Symbols are used by macros to define names that only they
// can access.
symbol MY_SYMBOL_NAME;

struct [MY_SYMBOL_NAME](Type1, Type2);

use foo::bar::[MY_SYMBOL_NAME]::baz;

// Imports a syntactical macro from a script.
macro myName in <const expr path>;
```

Module items may be referenced as either their local name (if defined in the current module or imported) or a module path of the form:

```
crate::path::to:Item                  // Relative to the current crate.

::other_crate_name::path::to::Item    // Relative to the root of a crate named "other_crate_name".

super::path::to::Item                 // Relative to the parent module. Can appear anywhere in the path.

^^::path::to::Item                    // Relative to the nth ancestor where "n" is the number of carets. Can appear
                                      // anywhere in the path.

StructName::path::to::Item            // Relative to the associated module of a type.
                                      // If the type is not-concrete (i.e. bound by a generic), this operation must be
                                      // wrapped in an "unsafe" block because it violates substitution failure principles.
                                      // Standard library macros can provide userland mechanisms to make this access safe.

typeof(value_name)::path::to::Item    // "typeof" expressions, which resolve to the type of the specified variable/field,
                                      // can be used in place of a type. The same rules regarding generic substitution
                                      // apply here as well.
```

Every item in a module can be associated with an item defined in its ancestor modules. Associating items with a module
allows them to be accessed as sub-items of the item (e.g. `MyStruct::associated_function`), ensures that extension
functions can be called if the parent item is in scope (described later), and allows users to decompose generic types through their
associated modules, enabling simple generic polymorphism.

```
pub struct MyStruct {
    foo: u32,
}

attach(MyStruct) fn new() -> MyStruct {
    MyStruct {
        foo: 32,
    }
}

fn main() {
    val my_instance = MyStruct::new();
}
```

All of these items can be tagged with attributes which may be visible to the Bare Crew compiler or the runtime:

```
//! Inner documentation attribute
@!inner_attribute_for_file_module(params here)

/// Documentation attribute
struct Documented;

@repr("union")
struct MyUnion {
	variant_1: u32,
	variant_2: i32,
}

@my_custom_metadata(with, args)
fn my_function() {}

@outer_attribute_for_inline_module
mod my_module {
	@!inner_attribute_for_inline_module
}
``` 

All of these items can also be tagged with a visibility qualifier:

```
pub struct MyPublicStruct;

struct MyPrivateStruct;

pub(only::visible::to::this::module) struct SemiPublicStruct;
```

All items can be tagged with the `global` qualifier, indicating that they can be added to the prelude of all descendant modules.

```
global use my::crate::prelude::{some_function, SomeStruct};

mod child {
	fn do_something() -> SomeStruct {
		some_function()
	}
}
```

Structs, enums, functions, type aliases, and propositions can all be parameterized by a generic parameter. Templating information is preserved in the compiler's binary output to allow individual runtimes to decide how to monomorphize the items. Type constraints
can be expressed as an arbitrary proposition over the type parameters using `where`-style generic constraints. All type substitutions must be valid (except when using unsafe non-concrete module access) at both the type checking and generic unification levels.

```
use core::intrinsics::{isUnsignedNumber, canCompare};

new prop myOtherProp<T> = false;
add prop myOtherProp = isUnsignedNumber(T);
add prop myOtherProp = T is i32;

fn addOne<T>(value: T) -> T
where
    myOtherProp<T>
{
    value + 1
}

fn subOne<T>(value: T) -> T
where
    myOtherProp<T>
{
    value - 1
}

fn min<T>(a: T, b: T) -> T where
    canCompare<T> and
	myOtherProp<T>
{
	if addOne(a) < addOne(b) { addOne(subOne(a)) } else { b }
}

@proc_name("rt_main")
fn main() {
	// Explicit parameterization
	let _ = min<u32>(4, 3);
	let _ = min<i32>(4, 3);

	// Implicit parameterization (not always possible)
	let _ = min(4u32, 3u32);
	
	// Implicit explicit parameterization (also not always possible)
	let _ = min<_>(4u32, 3u32);
}
```

Propositions are binary expressions on types. Their syntax allows them to encode first-class logic, which works well with Bare Crew's automated theorem proving system. Here are the logical connectives provided:

```
<prop expr> =
	true |
	false |
	unknown |
	if <prop expr> { <prop expr> } else { <prop expr> } |
	forall(<term name>) { <prop expr> } |
	exists(<term name>) { <prop name> } |
	// Rules appearing earlier in this list have higher
	// associativity than those lower down.
	<term> + <term> |
	not <prop expr> |
	<term> is <term> |
	<prop expr> or <prop expr> |
	<prop expr> and <prop expr> |
	<prop expr> nor <prop expr> |
	<prop expr> xor <prop expr>
```

Functions can be tagged with `extern` and leave them to be implemented by the compiler or the runtime:

```
@lang_intrinsic("ptr_to_usize")
extern fn ptr_to_usize<T>(ptr: *const T) -> usize;

// Sorry, no `usize_to_ptr`. Provenance rules are too tricky!
```

There is no `const` qualifier for functions. All functions are automatically `const` callable:

```
fn addOne(x: u32) -> u32 {
	x + 1
}

const MY_ARRAY: [u32; addOne(4)] = [1, 2, 3, 4, addOne(5)];
```

The Bare Crew runtime exposes the following primitive types:

```
// An entirely immutable pointer
*const T

// A unique mutable pointer
*unique T

// A potentially shared mutable pointer
*mut T

// A statically sized array
[T; <const size expr>]

// A bunch of numeric primitives
u8, i8    // Signed an unsigned 8 bit integers.
u16, i16  // Signed an unsigned 16 bit integers.
u32, i32  // Signed an unsigned 32 bit integers.
u64, i64  // Signed an unsigned 64 bit integers.
f32       // A 32 bit floating point number.
f64       // A 64 bit floating point number.
bool      // A boolean

// The following are special lang-items in the form of structs

// Used to enable mutability inside a "*const T" pointee.
struct Cell<T>(T);

// A non-zero integer.
struct NonZero<T>(T)
where
    isPrimitiveNumber<T>;
``` 

Function bodies take the form of a block expression, which is a mix of statement-style expressions, module items, and a single trailing return expression. Expressions can take the following form:

```
// TODO: Describe the Rust-like expression grammar.

// Converts a proposition to a `const` bool.
prop(PropHere)
```

Bare Crew supports tagged [UFCS](https://en.wikipedia.org/wiki/Uniform_Function_Call_Syntax) as its primary way of supporting method-call syntax. Functions whose first parameter is qualified with `this` may be called with `object.functionName(...)` and `object.functionName` syntax so long as the function can be unambiguously resolved to that `functionName` and the function is in scope (or in the attached module of an in-scope item). 

```
pub struct MyClass {
    foo: u32,
    bar: u32,
}

fn doSomething<T>(this self: T)
where
    crew_lang::core::inherits(T, MyClass)
{
    val self = crew_lang::core::deref<T, MyClass>(self);
    
    // (do computations with self here)
}

fn my_property(this self: MyClass) {
    self.foo + self.bar
}

fn myFunction<T>(value: T)
where
    crew_lang::core::inherits(T, MyClass)
{
    // "MyClass::doSomething" is in scope and is the only function which can
    // be called for all valid choices of "T". 
    value.doSomething();
    
    // match_prop! is a standard library macro which allows users to match
    // exactly one branch depending on the propositions.
    val my_value = core::match_prop! {
        T is MyClass: value.my_property,
        _: 0,
    };
}
```

Base Crew is tokenized by groups, forming a token tree. This allows the user to invoke macros that haven't been declared yet at the expense of always forcing braces in a token stream to be balanced. Macros are defined as functions on these token trees which return a replacement token tree. Each token has two scopes attached to it, a diagnostic scope and any number of lexical scopes. The mandatory diagnostic scope indicates the source span that should be highlighted when producing compiler diagnostics of the produced code. Diagnostic scopes may also be entirely fabricated and point to fake macro-generated syntax. Lexical scopes indicate module and function contexts where a given source span may derive its named items. Macros may also introduce new programmatically generated macros into the scope by introducing the special `ProcMacroDef` token. These macros can be called in the following ways:

```
my_macro!(token tree {here})

my_macro![token tree {here}]

my_macro! {
	token tree {here}
}

my_macro! { scope_1 } ~ (scope 2) ~ [scope 3]
```

Language authors may also define module macros which are invoked as if they were module items in a module. Unlike regular import macros, these must be defined before their use. These macros may also define custom binary operators (e.g. the `:>` inheritance constraint in Crew) for use inside propositions or expressions.

**TODO:** How can users write const-exprs that get resolved at the per-generic-instantiation level?

**TODO:** Describe core language overloads.

**TODO:** Describe associated proposition modules.

**TODO:** Generically varying structs (e.g. thin v.s. wide pointers depending on the size proposition)

**TODO:** Document `unsafe` markers (especially for propositions)

## License

Unlicensed for now but certainly something Free.
