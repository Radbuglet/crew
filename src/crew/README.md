# The Crew Programming Language

Crew is an object-oriented programming language which uses composition as its sole inheritance mechanism. Since no implementation exists yet, this document is simply a summary of the standout features of the language and some sample usage.

## Motivation

One of object-oriented programming's largest pain point is its typical lack of multiple inheritance. Language designers usually omit this feature because of [the diamond problem](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem), an ambiguity created when a subclass inherits two or more superclasses which in turn override a common virtual method in a shared ancestor class. Languages that do support multiple inheritance generally solve this problem by requiring the user to manually resolve the conflict. While this does enable multiple inheritance, this solution is both error-prone and subtly inflexible. Fundamentally, inheritance is flawed because there can only be one instance of each class in the class hierarchy. This causes problems when a class expects exclusive ownership over its superclasses (e.g. it expects that specific method overrides will be applied to the parent class or that no external actor will manipulate the underlying state), a guarantee that cannot exist because every class in the hierarchy is both a singleton and is publicly available to the entire class hierarchy.

To solve these issues, this language proposes composition semantics as its primary inheritance mechanism. Composition is a far more powerful model for behavioral inheritance than inheritance, as it allows users to specify which instance is being "extended" with the same granularity as one would reference any other field. Furthermore, components enjoy the same encapsulation mechanisms as normal class state. Composition is by no means unique to Crew, as the strategy is merely a consequence of being able to store class references in fields. However, composition in most other object-oriented programming languages is terribly half-baked as these languages do not include mechanisms to inherit component interfaces efficiently in a way that preserves definitional encapsulation. Furthermore, composition is generally less memory efficient than inheritance because each class in the component tree is allocated separately and requires a network of fields to reference component dependencies. Solving these pain points is the *raison d'être* of Crew.

## Exposition

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
        // a "to-do" comment. Reaching an ellipsis will cause the program to panic.
        ...
    }
}

class Entity {
    pub fn update_entity_state() {
        ...
    }
}

class Sprite {
    // All sprites must also be "subclasses" of "Spatial". There is no way to get a "Sprite"
    // instance without including its exposed sub-components.
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
    assert((entity as? Player).is_some());
}
```

Exposing in Crew is functionally more similar to manual interface delegation than to component resolution. To a user, this means that a parent class upcasted to a component class will still result in a reference the parent class, causing both references to have pointer equality and the ability to cast to each other. However, when a user directly accesses the exposed instance, they obtain a direct reference to the component instance which cannot be "down-casted" to its logical parents.

```
// "ptr_eq" is a compiler intrinsic for checking pointer equality.
use std::mem::ptr_eq;

class Parent {
    // Since components are just normal fields/properties, we can make them public as we would with any other
    // class member.
    pub out val child: Child;
}

class Child {}

fn eq(parent: Parent) {
    // Upcasting preserves reference identity.
    val child_casted: Child = parent;
    assert(ptr_eq(parent, child_casted));
    
    // Accessing the exposed class instance directly produces a physically different reference.
    val child_ref = parent.child;
    assert(!ptr_eq(child_casted, child_ref));
    
    // Users can only downcast to other components exposed by the same underlying class instance.
    // This cast is valid because "child_casted" points to the same object as "parent".
    assert((child_casted as? Parent).is_some());
    
    // This downcast is not valid because "child_ref" points to an instance of "Child", which does not
    // expose "Parent".
    assert((child_ref as? Parent).is_none());
}
```

This decision liberates users to expose components within multiple parent classes simultaneously and allows the parent class to change which class instance is being exposed while users are referencing it.

Exposition comes in two flavors: static and dynamic. Static exposition resolves the interface of the object from concrete types, omitting everything in the target interface that isn't part of that type. V-tables involving statically exposed components can be prepared ahead of time by the compiler and generally result in higher efficiency. Unqualified `out` is a form of static exposition and will infer the exposed type from the type of the field or parameter.

```
class Box<T> {
    pub out val target: T;
}

class DynBox<T> {
    // We expose "T" and "dyn" so users can statically access items from "T" but can also
    // access dynamic members of the "target" by casting this class.
    pub out(T, dyn) val target: T;
}

class Player {
    pub out val entity: Entity;
    
    // Crew does not have default constructors. Instead, we define a static method to act as the constructor.
    pub static fn new() -> Self {
        Self {
            entity: Entity { ... },
        }
    }
    
    pub fn player_method() {
        ...
    }
}

class Entity {
    ...
    
    pub fn entity_method() {
        ...
    } 
}

fn main() {
    val player = Player::new();
    val player_as_entity = player as Entity;
    
    val static_box = Box<Entity> { target: player_as_entity };
    val dynamic_box = DynBox<Entity> { target: player_as_entity };
    
    // The static box only exposes the "Entity" and its subcomponents.
    assert((static_box as? Player).is_none());
    assert((static_box as? Entity).is_some());

    static_box.entity_method();
    
    // The dynamic box exposes the entire interface of its target, including both "Player" and "Entity".
    assert((dynamic_box as? Player).is_some());
    assert((dynamic_box as? Entity).is_some());  // This can be proven statically. 
    
    // We can only *statically* access "Entity" methods on the "dynamic_box".
    dynamic_box.entity_method();

    // However, we can dynamically cast the box to include "Player".
    val dynamic_box_with_player = dynamic_box as! _ + Player;
    dynamic_box_with_player.entity_method();
    dynamic_box_with_player.player_method();
}
```

Static exposition qualifiers are closed by default, meaning that they cannot be overridden by any other closed exposed components. This has the useful encapsulation property of allowing users to guarantee that a class exposing a parent class will necessarily also expose its exact sub-component instance.

```
class Root {
    out val container: Container;
    
    // This line does not compile because it conflicts with "Container"'s exposed "Bindable" instance.
    // out val my_bindable: Bindable;
}

class Container {
    pub out val bindable: Bindable;
}

class Bindable {
    pub var is_bound: bool;
}

fn test(container: Container) {
    // The passed class' Bindable component is guaranteed to be Container's instance because the
    // Container exposes the Bindable in a closed-manner, meaning that if the Container shows up
    // in the component tree, so too does its Bindable.
    assert((container as Bindable).is_bound == container.bindable.is_bound);
}
```

Users can allow exposed components to be overridden by qualifying it with the `open` qualifier. Non-`open` properties will always override `open` ones. In the case where two or more components collide but all of them have the `open` qualifier, the user can re-expose the instance they want to expose or explicitly remove the exposed class from the exposed class list (done with the class removal `-` syntax). In cases where generic parameters are exposed statically, the type checker cannot prove that the concrete type of the parameter will not interfere with existing components, requiring the `open` qualifier.

```
// TODO: Example
```

**TODO:** Document generic parameter variance, its limits when used in conjunction with static exposition, and potential variance ambiguities

## Static Input Linking

To address the efficiency concerns of composition, Crew introduces two additional qualifiers: `in` and `impl`. `in` and `impl` come in pairs. `in` defines a dependency on a member and `impl` provides an implementation of that dependency. By declaring dependencies using this new system, the compiler can store specializations in the form of relative component offsets as part of the object's v-table, making component dependencies zero-cost in terms of their memory footprint.

```
class Spatial {
    pub val pos: Vec3;
}

class PhysicalObject {
    // "in" defines a prototype for a class member to be provided by an "impl" qualified member
    // in a parent class.
    in val spatial: Spatial;
    
    val velocity: Vec3;
    
    // There is no notion of a default constructor in Crew. Instead, users provide a suite of
    // static constructor methods.
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

class PhysicalRenderer {
    in val sprite: Sprite;
    in val physical: PhysicalObject;
    
    ...
}

class Player {
    // The "impl" qualifier takes a list of paths to the input members that require a concrete
    // member. If any input members of a class' children do not receive their value, the parent
    // class will automatically forward these inputs to its consumers.
    impl(sprite.spatial, physical.spatial)
        // Any field/property, so long as it provides a superset of the guarantees required by
        // the "in" prototypes, is accepted here.
        var spatial: Spatial;
    
    out val sprite: Sprite;
    out val physical: PhysicalObject;
    out val physical_renderer: PhysicalRenderer;
    
    // Users can also use the "<qualifier> on <member>" syntax to separate impl statements and
    // component definitions
    impl(physical_renderer.sprite) on sprite;
    impl(physical_renderer.physical) on physical;
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
            assert(self.stage == Stage.Stopped);
            self.stage = Stage.PreInit;
            self.base_handler.onInit();
        }
        
        fn _onEnable() {
            assert(self.stage == Stage.PreInit);
            self.stage = Stage.Running;
            self.base_handler.onEnable();
        }
        
        fn _onDisable() {
            assert(self.stage == Stage.Running);
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

**TODO:** Document scope-qualification in `in` to limit who can implement a member prototype.

**TODO:** Document `IDynamicExposer<T>` and `IDynamicallyCall<T>` and give examples of their use.

## Implementation Status

See [TODO.md](TODO.md) for more details on the MVP compiler's implementation status.

