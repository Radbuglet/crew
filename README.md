# The Crew Programming Language

Crew is an object-oriented programming language which uses composition as its sole inheritance mechanism. Since no implementation exists yet, this document is simply a summary of the standout features of the language and some sample usage.

## Motivation

One of object-oriented programming's largest pain point is its typical lack of multiple inheritance. Language designers typically omit this feature because of [the diamond problem](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem), an ambiguity created when a subclass inherits two or more superclasses which in turn override a common virtual method in a shared ancestor class. Languages that do support multiple inheritance generally solve this problem by requiring the user to manually resolve the conflict. While this does enable multiple inheritance, this solution is both error prone and subtly inflexible. Fundamentally, inheritance is flawed because there can only be one instance of each class in the inheritance hierarchy. This causes problems when a class expects exclusive ownership over its superclass (e.g. it expects that specific method overrides will be applied to the parent class or that no external actor will manipulate the underlying state). Inheritance fails to provide this encapsulation because every class in the hierarchy is both a singleton and is publicly available to the entire class hierarchy.

To solve these issues, this language proposes composition semantics as its primary inheritance mechanism. Composition is a far more powerful model for behavioral inheritance than inheritance, as it allows users to specify which instance is being "extended" with the same granularity as one would reference any other field. Furthermore, components enjoy the same encapsulation mechanisms as normal class state. Composition is by no means unique to Crew, as the strategy is merely a consequence of being able to store class references in fields. However, composition in most other object-oriented programming languages is terribly half-baked as these languages do not include mechanisms to inherit component interfaces efficiently in a way that preserves definitional encapsulation. Furthermore, composition is generally less memory efficient than inheritance since each class in the component tree is allocated separately and require a network of fields to reference component dependencies. Solving these pain points is the *raison d'être* of Crew.

## Composition

Composition in Crew is handled with three main qualifiers: `out`, `in`, and `impl`.

The `out` keyword is technically the only keyword required to start composing classes. It *exposes* the interface of the specified property's type as part of the interface of the parent class. Since `out` applies to the entire class' interface, sub-components will be exposed all the way to the top of the *component tree*.

```
class Player {
    // Expose the entire "Entity" class.
    out val entity: Entity;
    
    // Expose only the "Spatial" sub-component of the "Sprite".
    out(Spatial) val sprite: Sprite;
    
    // "pub" means that the function is globally available. Without "pub", this method would
    // be class internal.
    pub fn update_player_state() {
        // This ellipsis is syntax for an unimplemented method with similar semantics to
        // a "TODO" comment. Reaching an ellipsis will cause the program to panic.
        ...
    }
}

class Entity {
    pub fn update_entity_state() {
        ...
    }
}

class Sprite {
    out val spatial: Spatial;
    
    pub fn render() {
        ...
    }
}

class Spatial {
    pub fn move_up() {
        ...
    }
}

fn tick_player(player: Player) {
    player.update_player_state();  // Call to methods in "Player".
    player.move_up();  // Call to component methods in "Spatial".
    update_entity(player);  // Upcast to "Entity" component.
}

fn update_entity(entity: Entity) {
    entity.update_entity_state();
    
    // Downcast to the parent "Player" class.
    assert!((entity as? Player).is_some());
}
```

**TODO:** Document `out` collision resolution and its useful encapsulation properties.

**TODO:** Document generics, variance and its ambiguity, determine precise behavior of exposing generic parameters.

Components exposed using `out` are *inlined* into their parent's definition, with all inlined components in a component tree being sharing the same identity. This allows users to downcast components back into any other component with the same identity, at the expense of limiting users to expose the class under one component tree (although the same component can be exposed several times, so long as it is exposed with the same identity).

**TODO:** Determine how to handle the transfer of ownership efficiently (introduce an `inline` qualifier for the stack member?).

Users can sacrifice the ability to be down-casted into by opting out of `out`'s inlining and identity merging behavior. This is useful for defining wrapper classes that expose the interface of the underlying type.

```
// The "out" qualifier in front of the generic parameter "T" means that the parameter is
// covariant.
class WithCounter<out T> {
    // "ref" qualifies "out" and prevents the default behavior of inlining the value.
    out ref val target: T;
    
    // This is just a normal mutable property.
    var count_: u32;
    
    // The "static" qualifier turns this method into a static method. There are no default
    // constructors in Crew.
    pub static fn new(target: T) -> Self {
        Self {
            target,
            count_: 0
        }
    }
    
    // "count" is a readonly property.
    pub val count: u32 => self.count_;
    
    pub fn add_one() {
        self.count_ += 1;
    }
}

class TargetClass {
    pub static fn new() -> Self {
        Self {}
    }
    
    pub fn do_something() {
        ...
    }
}

fn example() {
    val target = TargetClass.new();
    
    val wrapper = WithCounter.new(target);
    wrapper.do_something();  // Call to method in target "do_something".
    wrapper.add_one();  // Call to wrapper method "add_one".
    
    val unwrapped: TargetClass = wrapper;  // Upcast to "TargetClass".
    // We cannot downcast "unwrapped" back to the "WithCounter" wrapper instance because it
    // isn't part of the "target"'s identity.
    assert!((unwrapped as? WithCounter).is_none());
}
```

On the flip-side, users can inline a class without exposing it using the `inline` field qualifier. Inlined fields have full down-casting support.

**TODO:** Should we make identity merging optional? (e.g. for `Vec3`)

```
class Parent {
    pub inline val child: Child;
}

class Child {}

fn example(parent: Parent) {
    // Fetch the child.
    val child = parent.child;
    
    // Down-cast back to the parent
    assert!((child as? Parent).is_some());
}
```

---

To address the efficiency concerns of composition, Crew introduces two new qualifiers: `in` and `impl`. `in` and `impl` come in pairs. `in` defines a dependency on a member and `impl` provides an implementation of that dependency. By declaring dependencies using this new system, the compiler can store specializations in the form of relative component offsets as part of the object's v-table, making component dependencies zero-cost in terms of their memory footprint.

```
class Player {
    // The "impl" qualifier takes a list of paths to the input members that require a value.
    // If any input members of a class' children do not receive their value, the parent class
    // will automatically forward these inputs to its consumers.
    impl(physical.spatial, sprite.spatial)
        // Any field/property, so long as it provides a superset of the guarantees required by
        // the "in" prototypes, is accepted here.
        inline var spatial: Spatial;
    
    out val physical: PhysicalObject;
    out val sprite: Sprite;
}

class Spatial {
    pub val pos: Vec3;
}

class PhysicalObject {
    // "in" defines a prototype for a class member to be provided by an "impl" qualified member
    // in a parent class.
    in val spatial: Spatial;
    
    // Crew doesn't need an explicit datatype defining mechanism. We can just inline the value
    // as part of the class.
    inline val velocity: Vec3;
    
    pub static fn new() -> Self {
        Self {
            velocity: Vec3.zero(),
            // input member fields do not require initialization
        }
    }
    
    pub fn move() {
        // We can access input member prototypes as we would any other class member.
        self.spatial.pos += self.velocity;  // Yes, we support operator overloading.
    }
}

class Sprite {
    in val spatial: Spatial;
    
    pub fn draw() {
        draw_sprite_at(self.spatial.pos);
    }
}
```

Input members behave almost identically to normal members, which means that they can, among other things, be publicized with the `pub` qualifier. This allows Crew to provide an interface mechanism solely through classes.

```
class Plugin {
    out val base: PluginBase;
    
    // This is an "impl" block. The wildcard specifies that members will be automatically
    // associated with their identically named input members, ignoring any leading underscores.
    impl(base.base_handler.*) {
        // Since "impl" members don't have to be public, this allows us to implement an internal
        // interface without exposing it or creating any internal classes.
        
        fn _onInit() {
            ...
        }
        
        fn _onEnable() {
            ...
        }
        
        fn _onDisable() {
            ...
        }
    }
}

class PluginBase {
    var stage: Stage;
    in val base_handler: IPluginHandler;
    
    pub static fn new() -> Self {
        Self {
            stage: Stage.Stopped,
            // input member fields do not require initialization
        }
    }
    
    // "impl" blocks can be affixed directly to the field requiring the inputs. This is equivalent
    // to defining the impl block "impl(_handler.*)".
    out val _handler: IPluginHandler impl {
        fn _onInit() {
            assert!(self.stage == Stage.Stopped);
            self.stage = Stage.PreInit;
            self.base_handler.onInit();
        }
        
        fn _onEnable() {
            assert!(self.stage == Stage.PreInit);
            self.stage = Stage.Running;
            self.base_handler.onEnable();
        }
        
        fn _onDisable() {
            assert!(self.stage == Stage.Running);
            self.stage = Stage.Stopped;
            self.base_handler.onDisable();
        }
    }
}

class IPluginHandler {
    pub in fn onInit();
    pub in fn onEnable();
    pub in fn onDisable();
}

enum Stage {
    Stopped,
    PreInit,
    Running,
}
```

**TODO:** Delegate types?



## Metaprogramming

**TODO:** Routers, proxies, macros, reflection, and source transformers.



## Runtime

**TODO:** Determine...

- Memory model
- Component casting
- Input monomorphization (opt-in dependency monomorphization? Allow partial monomorphization for dispatch categorization?)
- Instance embedding semantics
- VM interop and isolation

