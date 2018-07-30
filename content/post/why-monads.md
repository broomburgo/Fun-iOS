+++
title = "Why Monads?"
date = 2018-07-30T15:00:00+02:00
+++

There's some talk going on in the [Swift Forums](https://forums.swift.org/t/higher-kinded-types-monads-functors-etc) about whether it could be useful or not to add the notion of [Higher-Kinded Types](https://en.wikipedia.org/wiki/Kind_(type_theory)) (from now on, HKTs) to the Swift's type system. This discussion has been going for a while in the Swift community, and there seems to be a lack of practical examples to sustain the need for such an advanced feature. Unfortunately, HKTs are in the area of things that become obvious if you have them and use them every day, but tend to remain obscure if you have to work without them: that's because HKTs can shift one's way of thinking about programming, abstractions and computation in general.

Also, while *functors* or *monads* are HKTs, you don't need higher-kinded typing to design and implement types that behave like them: in languages that have HKTs a `Monad`, for example, is basically an interface, like a Swift `protocol`, with some extra power that Swift, at the present moment, cannot deliver. But you can implement the `Optional<Wrapped>` type in Swift, and work with it successfully, without knowing, nor needing to know, that `Optional<Wrapped>` is a monad. I happen to think that, if Swift had the notion of HKT in its type system – for example, if we could express the concept of `Monad` generically (maybe with some kind of protocol) - then we'd be able to write practical, pragmatic, useful code that's only unlocked by the power of HKTs.

In this article I'm going to explore that usage of monads for practical code, and the power unlocked by their composition: in doing so, I'm also going to give my personal introduction to monads, something that you can find by the dozen by googling "monad introduction". There's really no point in linking other introductions here, for I want to try and give my own, and they're so easy to find; but I'll refer to a couple of resources that I consider particularly valuable, and helped me a lot at the time:

- [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html): a wonderful explanation of the basics of 3 important category-theoretical concepts when applied to programming;
- [Don't fear the Monad](https://www.youtube.com/watch?v=ZhuHCtR3xq8): a lovely introduction to monads in which Brian Beckman traces a parallelism with the (even more) important concept of *Monoid*.

I'm also going to take the `Monoid` path to reach the `Monad` land, but in a different way; I'd like to do so to also try and give an explanation why we use the word *monad*, that seems pointlessly abstract: while the concept of `Monoid` is inevitably so abstract that there really couldn't be a more "descriptive name" for it (like trying to give a more descriptive name to the concept of "multiplication", you just learn it and move on), `Monad` is sometimes referred with more pragmatic-sounding names like "flatmappable" or "chainable". While I certainly think that using less scary and more *interface-like* sounding names for abstract concepts can be useful (for example, I'd probably give up the *functor* name, and use *mappable*), there are reasons why a monad is called like that and sounds like "monoid", and I think that understanding those reasons could really reveal some deeper truths about why these abstractions are important.

Let's start then. Brace yourselves.

## Why Monoids?

I think that the concept of [`Monoid`](https://en.wikipedia.org/wiki/Monoid) is the single most important idea that percolated from abstract algebra to computation: actually, abstract algebra is usually more concerned with [groups](https://en.wikipedia.org/wiki/Group_(mathematics)), while monoids seem to be particularly useful in software development, at least in my experience. This article is about monads, and my goal is not really to talk about monoids, but the concepts are deeply related, and I think that an understanding of the properties and usefulness of monoids will help in truly understanding what monads are about.

`Monoid` encapsulates the idea of *merging two things together into a single one, with the possibility that one of those things is irrelevant*. For example, we concatenate strings: we take two strings, put them together, and generate a single string. This composition operation is *binary*, because it takes two and gives one. But if I'm concatenating strings and I want to ignore one of them, a simple way is to just use an empty string: `"Hello "` plus `"World!"` gives `"Hello World!"`, but `"Hello "`  plus `""`, or `""` plus `"World!"` gives back the first or second string like if nothing happened. It turns out that `String` is a `Monoid` in respect to string concatenation, and the *irrelevant*, or *neutral*, instance (let's call it `empty`), that is, the empty string, is ignored in the composition operation.

Another example is `Array<Element>`, in which again concatenation is the "putting two things together" operation, and an empty array is the neutral instance. Here's another one: `Bool` is a monoid, if we consider the `&&` (*AND*) operation, and the `true` instance. **But** `Bool` can **also** be a `Monoid` if we consider the `||` (*OR*) operation, and the `false` instance: what's going on here?

Let's first give a more formal definition of a `Monoid` protocol, something that we can fully do in Swift right now:

```swift
protocol Monoid {
    func append(_ other: Self) -> Self
    static var empty: Self { get }
}
```

It's common for implementations based on abstract algebra to employ an operator in place of the `append` method, like the "diamond" `<>` operator; I'm not a great fan of using operators in Swift because of slow compilation times (the type checker really can't stand too may concatenated operators), and in general I like to have at least the choice to use a method instead of an operator: there's a place for custom operators, but for now let's go with methods instead.

Anyway, the `append` method takes another instance of `Self`, and combines it with the `self` instance, to get a new one: from two, we get one, so it represents a binary operation. Then, there's the `empty` static property, which is the "neutral" fixed one. There are 3 requirements for a type to properly conform to the `Monoid` protocol:

- the `append` method **must be total**: no preconditions, no fatal errors, nothing weird going on; if the method is called, it must not crash;
- the `append` operation must be **associative**, that is, `a.append(b).append(c)` must be exactly the same as `a.append(b.append(c))`, that is, however I put the parentheses, the final result is the same;
- the `empty` instance must be **neutral** in regard to the `append` operation, which means that `a.append(empty)` and `empty.append(a)` must be the same and exactly equal to just `a`.

Notice that `append` has a generic-sounding name because **it is** completely generic: the meaning (semantics) of `append` will depend on the concrete type conforming to `Monoid`. Thus, `append` for `String` and `Array` will mean "concatenation", which is associative. But what about `Bool`? Before dealing with that, let's try and understand why it makes sense to have this kind of protocol in the first place, with an example.

Let's assume we have a current user session going within an app. The `Session` object could be represented by a couple of structs like the following:

```swift
struct UserSession {
    var token: String?
    var identity: UserIdentity
}

struct UserIdentity {
    var identifier: String
    var updatedAt: Date
}
```

A session is identified by a token (initially, `nil`) that, when updated, is returned by a server on the next request; there's also an "identity", characterized by an identifier and an update date (that's because we only ever care about the most recent identity). Both the token and the identity could be updated at any time, in response to a number of events.

To handle the update, we could design an object that we pass around as a mutable dependency, with ad-hoc methods for the various manipulations. This way we centralize the manipulation, but we need to pass around a dependency, and "expressing the update" means really "calling a method on some object", which will probably require the definition of an interface, some boilerplate, and a "contract" that will [complect](https://www.youtube.com/watch?v=rI8tNMsozo0), that is, bind together two contexts that shouldn't necessarily be related. 

Alternatively, we could *express* the kind of update we want to do with functions that operate on `UserSession`: this way we can declaratively produce these updates as functions of type `(UserSession) -> UserSession`, that can be handled in a different context, and that's a huge win, but the function type doesn't carry any meaning by itself, and we'll need to rely on the function names to understand what's going on; also, in the context where these functions are called, there's no memory of what kind of update we're going to perform.

These strategies have pros and cons, but are both characterized by a potentially crucial detail: the `UserSession` type **in itself** doesn't express the business logic related to its update. We could add methods (mutating or not) for expressing this logic, but these would be redundant and essentially duplicate some logic already expressed in both strategies. An interesting alternative could be to express the update logic **at the type level**: we could represent the way to update all parts of a `UserSession` with specific types. Then, when *merging* instances of those types, each one of them will express specific semantics of update.

For example, we could design an `Update<A>` type for which the merge on two instances will *favor* the second of the two, while the `empty` instance will simply wrap `nil`:

```swift
struct Update<A>: Monoid {
    let unwrap: A?
    
    init(_ value: A?) {
        self.unwrap = value
    }
    
    func append(_ other: Update<A>) -> Update<A> {
        return Update<A>(other.unwrap ?? self.unwrap)
    }
    
    static var empty: Update<A> {
        return Update<A>(nil)
    }
}
```

Equipped with this new type, we can redefine `UserSession` as follows:

```swift
struct UserSession {
    var token: Update<String>
    var identity: UserIdentity
}
```

With this definition we're saying that the token is an `Optional<String>` that will update by simply discarding the previous one if the next one is not `nil`.

For `UserIdentity` we can, on first approximation, give a custom implementation of `Monoid`.

```swift
extension UserIdentity: Monoid {
    func append(_ other: UserIdentity) -> UserIdentity {
        if other.updatedAt > self.updatedAt {
            return other
        } else {
            return self
        }
    }
    
    static var empty: UserIdentity {
        return UserIdentity.init(
            identifier: "",
            updatedAt: .distantPast)
    }
}
```

In this case, the `append` method will favor the most recent `UserIdentity`, and the `empty` instance has `distantPast` as date, so it will never be selected in an `append` operation.

Now that both the `token` and `identity` properties are monoids, we can trivially make the whole `UserSession` a `Monoid`, by simply updating both properties with the `append` method, and use both the `empty` static functions:

```swift
extension UserSession: Monoid {
    func append(_ other: UserSession) -> UserSession {
        return UserSession.init(
            token: self.token.append(other.token),
            identity: self.identity.append(other.identity))
    }
    
    static var empty: UserSession {
        return UserSession.init(
            token: .empty,
            identity: .empty)
    }
}
```

Equipped with these definitions, something magical happens: any time we want to express an update in `UserSession`, we simply need to produce a new one. For example, if we're updating just the token, we can produce a new session with a new token, and an `empty` user identity; if we're updating the identity, we can just yield `nil` for the token, and we'll be sure that the most recent identity will be kept in any case.

Interestingly, in an atomic process that contains multiple session updates, we can just collect all the updates (in order) and generate a single one in batch, with a `concatenated` method:

```swift
extension Sequence where Iterator.Element: Monoid {
    func concatenated() -> Iterator.Element {
        return reduce(.empty) { $0.append($1) }
    }
}
```

Notice that this method doesn't know anything about the fact that `Iterator.Element` is a `UserSession`: it just needs to know that it's a `Monoid`, so it has `empty` and `append`.

What about the `Bool` conundrum? Well, two different specific types will express the two ways of "composing" booleans, let's call them `And` and `Or`:

```swift
struct And: Monoid {
    let unwrap: Bool
    
    init(_ value: Bool) {
        self.unwrap = value
    }

    func append(_ other: And) -> And {
        return And(self.unwrap && other.unwrap)
    }
    
    static var empty: And {
        return And(true)
    }
}

struct Or: Monoid {
    let unwrap: Bool
    
    init(_ value: Bool) {
        self.unwrap = value
    }

    func append(_ other: Or) -> Or {
        return Or(self.unwrap || other.unwrap)
    }
    
    static var empty: Or {
        return Or(false)
    }
}
```

This might seem pointlessly complicated when confronted to simply use `&&` and `||`, and in this particular case it probably is, but I hope I've made a compelling argument for using types that express specific semantics of binary composition. `And` and `Or` allow to embed boolean composition in the monoid framework, and by constructing types that embed binary, associative composition, a world of possibilities unfolds: what I've shown here is just the tip of the iceberg.

Like basic mathematical operations that eventually yield complex expressions, the mere act of merging two things together, resulting in a third thing of the same type, is a very basic operation that can unlock sophisticated abstractions constructed from simple tools. It all accounts to properly express our types in a way that favors combination; this way we don't need to define many ad-hoc methods, that could change over time and disrupt interfaces and contracts: expressing combination semantics at the type level is the *ultimate encapsulation strategy*.

## From instances to types

Monoids are about putting together instances of types: if a type conforms to the `Monoid` protocol, it means that we can merge two of its instances into a single instance of the same type, and that there exists a very specific instance that will act neutrally towards this composition. Now, here's the key question: can we do the same thing with **types**? What I mean is this: is there a way to express the *composition of types* in a monoid-like way? Notice that to be *monoidal*, the types should somehow share something (like instances of a monoid share the fact that they're of the same type), and there should exist some kind of *neutral type* in respect to their composition. But what *composition* might even mean for types?

Well, an option could be using [*algebraic data types*](https://broomburgo.github.io/fun-ios/post/algebraic-datatypes-for-contextual-and-conditional-computation/), like `Product<A,B>` to compose two concrete types: `Int` and `String` would compose into `Product<Int,String>`, but this is not the kind of composition we're talking about, because `Int`, `String` and `Product<Int,String>` are different, unrelated (well, not completely) types. This is not the monoidal composition we're looking for.

What about *generic* types? At this point it's probably better to give a new definition, that is, *type constructor*. A type constructor, in general, is a mapping from one or more types to a constructed, final type: for example, a *unary* type constructor takes a single type and returns a new single type, different in general from the first one, but typically related. In Swift, type constructors are represented by **generic types**. Think about it: `Optional<Wrapped>`, for example, is a unary type constructor, because if I give it a type, say `String`, it will yield a specific type `Optional<String>`, while if I give it another, different type, say `Array<Int>`, it will yield another, specific, different type `Optional<Array<Int>>`. It turns out that a type constructor, that is, a generic type in Swift, has a monoidal representation. A type constructor could also be *binary*, like `Dictionary<Key,Value>`, or in general *n-ary*, but we're really only concerned with one of the generic parameters, fixing all the others: so, the monoidal representation will refer to only one of the type arguments (the generic parameters).

The "composition" we're looking for here is the *nesting* of the same type constructor, like for example `Optional<Optional<Wrapped>>` or `Array<Array<Element>>`, or in general `Generic<Generic<A>>`: to be monoidal, this composition should yield a type identical to the initial, non-nested one; it should also define some `Generic<A>` *neutral* type; and it should respect the associativity and neutrality laws. To summarize, if a generic type has this kind of monoidal composition, it must define the following:

- an *append* mapping in the form of `Generic<Generic<A>> -> Generic<A>`;
- a *neutral* element `Generic<A>`;
- the *append* mapping should be associative, that is, the path `Generic<Generic<Generic<A>>> -> Generic<Generic<A>> -> Generic<A>` should be the same whether we merge the first and the second first, or the second and the third first;
- the *neutral* element, if nested into a `Generic<_>`, or if we nest a `Generic<A>` into it, should merge into a `Generic<A>` without any change.

If these four requirements hold, `Generic<A>` is a **monad**. Notice that we're really requiring the same things that we required for monoids, even if this time we're dealing with the nesting of generic types: if you think about it, you can see that it's still really about putting two things together to get a single one, and a neutral element towards this composition. This is also why *monad* and *monoid* sound similar: a monad is a monoid *lifted* into the world of type constructors.

> From a mathematical standpoint, monads are more generic than that, because they transcend the concept of *type* or *generics*, but we're interested in the representation of monads in Swift, and generic types provide that. Still, the monads we're talking about are more related to the monads in category theory than "functors" are; that's why I'd keep the *monad* word, but lose *functor* in favor of *mappable*.

Let's try and write some code to equip `Generic<A>` with monad features, and by doing that we'll express the *append* operation with a `joined` method (because we're joining a nested instance into a base one), and the *neutral* element with a `pure` method, that returns the "purest" instance possible for the type, the one that's going to be neutral towards joining:

```swift
extension Generic {
    func joined <T> () -> Generic<T> where A == Generic<T> {
        /// some code
    }
 
    static func pure(_ value: A) -> Generic<A> {
        /// some code
    }
}
``` 

Unfortunately, in trying to give a practical representation in terms of Swift code of what a monad is about from an abstract standpoint (like in defining a `Monad` protocol), we hit a brick wall; we really can't do that in Swift, because as you can see, the `joined` method for `Generic<A>` returns a `Generic<T>`, that is, **same** type constructor, but **different** generic parameter: if we used `Self`, it would mean `Generic<A>` again, and we cannot write something like `Self<T>` in Swift. That's because we don't have HKTs in Swift, that is, what this article is really about. The fact that we cannot generically talk about monads in Swift code has many consequences: for now, I'll simply observe that things like `concatenated` for monoids is not expressible in Swift for monads, but in the rest of article we'll see a few practical examples of why this can be problematic.

It's frequent to see monads expressed in terms of a `flatMap` or `bind` method, that for the generic type would have this kind of shape:

```swift
extension Generic {
    func flatMap <T> (_ transform: (A) -> Generic<T>) -> Generic<T> {
            /// some code
    }
}
```

This representation is frequently more useful, but it's completely equivalent to the `joined` method I presented before: to implement this, one can simply `map` and then `joined`. Or otherwise, one can implement `flatMap`, and then implement `joined` by passing `pure` to `flatMap`: as I said, the representation are equivalent. But mathematically, monads are defined in terms of the operation represented by `joined`, and the latter I think is more convenient to implement (because there's no transformation with an new parameter) and more rigorous.

I can always `map` because a monad is always also a *functor*, that for us is simply *something that can be mapped, by respecting some rules*, and I don't really want to talk about functors right now, but we're going to need a tool, the `map` function, so let's get it out of the way and define it:

```swift
extension Generic {
    func map <T> (_ transform: (A) -> T) -> Generic<T> {
        /// some code
    }
}
```

`map` transforms the *content* of the type, thus changing the generic parameter.

## Special effects

Up to this point I treated monads as a construction based on the nesting of a generic type, but I didn't present any practical use case for it. In iOS development (or any other, really) with Swift, we sometimes end up with nested generics, for example `Optional<Optional<Something>>` or `Array<Array<Something>>`; at the level of the standard library, the former case is not really handled in any sensible way: either a double `if/guard-let`, or a double `!` are required to extract the value nested inside; but the latter case is cleanly handled with a `joined` method, basically the same method the I defined for a monad, with the difference that it returns a more efficient `FlattenBidirectionalCollection<[[Something]]>` (that could be easily converted into an `Array` if needed). Efficiency on collections is a cornerstone of the Swift standard library, thus many types in it are designed to allow for fast conversions and iterations, but the `Element` of a `FlattenBidirectionalCollection<[[Something]]>` is `Something`, like in `Array<Something>`.

For now we can observe that a `joined` method would make sense also for `Optional<Optional<Something>>`, with basically the same shape as `Array<Array<Something>>`: notice that the two implementations would be (and actually are) completely different, and they depend on the specific internal structure of the type. Now, are there more types for which this kind of operation would make sense? Let's consider for example the following class:

```swift
protocol HasCalories {
    var calories: Int { get }
}

class Animal<Food> where Food: HasCalories {
    func consume(_ food: Food) {
        /// some code
    }

    var consumedCalories: Int {
        /// some code
    }
}
```

The `Animal` class is parameterized over the `Food` eaten, and `Food` can be anything that `HasCalories`. If we nest the generics we get `Animal<Animal<SomeFood>>`, that is, an animal that eats animals that eat `SomeFood`: if we `joined` this, we would get `Animal<SomeFood>`, but what does it even mean? Is this animal eating `SomeFood`, or other animals? The `joined` operation is not something that you would do for every generic type. Here's another example: we could easily write a `joined` function on `Dictionary<Key,Dictionary<Key,SomeValue>>` to get `Dictionary<Key,SomeValue>`, but it wouldn't make any sense, because we would merge all the subdictionaries – in case of identical keys, some values would have precedence, so we'd lose the other ones – and we would probably lose some of the original keys.

It turns out that there are specific categories of generic types where the "joining" operation makes sense, and it's actually useful: types that represent *effects*. To my knowledge, there's no shared, widely adopted and authoritative source on the concept of *effect* in programming, in the sense that it eventually *becomes clear* in one's mind, but I wasn't able to find a straightforward clear-cut definition. An *effect* in programming and computation in general is any consequence of an operation that doesn't yield a direct informational value. For example, a simple division of `Double`s seem a straightforward operation, with nothing weird going on, but if we implement the function we discover that the reality is not that simple:

```swift
func divide(_ dividend: Double, by divisor: Double) -> Double {
    guard divisor != 0 else {
        fatalError("Cannot divide by zero")
    }
    
    return dividend/divisor
}
```

As you can see, this function crashes if we pass `0` as divisor: this means that the function is not *pure*, the function is **partial**, it doesn't just produce a straightforward informational value (the result of the division) even if the signature says so. There's an *effect* going on here, and I could ignore this and risk crashing, or I could leverage the Swift type system to somehow *encapsulate* the precondition, and go back to a pure function. Of course Swift has already a way to deal with this, the `Optional<Wrapped>` type:

```swift
func divide(_ dividend: Double, by divisor: Double) -> Double? {
    guard divisor != 0 else {
        return nil
    }

    return dividend/divisor
}
```

Now the function signature says it all: the operation could fail, so I'm returning an `Optional<Double>`. This is a pure, straightforward function, that expresses its *effect* (that is, *partiality*) clearly.

What if I want to divide twice? Something like `divide(divide(42, by: 28), by: 21)` wouldn't compile, because the external `divide` (that's executed second) requires a `Double` dividend, but I'm providing a `Optional<Double>`. The solution is simple, and Swift has already a way to deal with this in the standard library:

```swift
divide(42, by: 28)
    .flatMap { divide($0, by: 21) }
```

`flatMap` *chains* the two effects together. With a `joined` method, I could do the same more explicitly:

```swift
divide(42, by: 28) /// yields Optional<Bool>
    .map { divide($0, by: 21) } /// yields Optional<Optional<Bool>>
    .joined() /// yields Optional<Bool>
```

As you can see, `flatMap` is generally more convenient to use, and that's why you usually find something like that in standard libraries: but we shouldn't forget where the concept comes from, that is, nesting multiple effects into a single one.

An implementation of `joined` for `Optional<Wrapped>` looks like this:

```swift
extension Optional {
    func joined <T> () -> Optional<T> where Wrapped == Optional<T> {
        switch self {
        case .none:
            return .none
        case let .some(value):
            return value
        }
    }
}
```

Notice that to make this work we need to *enter into* the `Optional`, navigate the structure, and produce the result. This code works for `Optional<Wrapped>`, so the implementation of a `joined` will ultimately depend on the effect involved.

`Array<Element>` is **also** a particular kind of effect: it models *non-deterministic* computation, where the result of some process can be one of many (even none), and in moving the computation forward, we should take into account all the possible values. When *flattening* an array of arrays, we need to navigate it, thus taking into account the effect that it represents thanks to a specific knowledge of its structure. Of course arrays are not generally used for this reason, but they could model this kind of computation if needed, so they can represent this *effect*.

An effect cannot be *unwrapped* or *executed* easily, at least in principle: for example, force-unwrapping an optional value can result into a crash. The monadic definition allows to represent the execution of effects in a safe and incapsulated way, thanks to the nesting of generics, and their flattening. The fact that we're remaining in the monadic context means that we can execute the effects *safely*, as a pure operation. For example, we've already seen that an operation like `(Optional<Int>) -> Int` is **not pure**, because either we pull numbers out of a hat, or we need to force unwrap; but if instead of `Int` we had `Optional<Int>` again, the operation `(Optional<Optional<Int>>) -> Optional<Int>` could be performed safely, and it would be **pure**, so it's a completely different situation, even if it looks like the previous one: the fact that `Optional<Wrapped>` is a monad makes this possible.

## The M word is here to stay

Effects are wild beasts, and using them makes understanding programs really hard, but we literally cannot do anything useful without effects, not even a simple division. It might feel like I'm repeating myself over and over again, but I really want to stress out this point: monads help with effects, because they allow to execute them safely within a particular context, and this is the case because `joined` is a pure operation. Here's a simple example, that will show a more direct relation between monads and effects, and will allow for the introduction of another monad: the `Effect<Value>` type.

The `Effect<Value>` type, also called *IO*, represents an *effectful* computation that produces an instance of type `Value`. It basically incapsulates a function of type `() -> Value`: because `Value` can be any type, and we have no information about it, it's completely impossible for this function to be *pure*. A function of type `() -> Int` *could be* pure, if for example always returned the number `42`, but that's just because we know the characteristics of the returned value's type: but if the returned type is completely generic, there's no way a function that takes *nothing* - actually, `()`, which is more like a *singleton*, traditionally, but quite erroneously called `Void` - returns *something*. But then, if I wanted to write to a function that returns a value of type `Value` taken from a globally mutable context (like a global variable), I could do it with a function of type `() -> Value` by referring the mutable context from within the body of the function. Another case could be executing some side effect on an external context (like persisting data), in which case I could use an even simpler function of type `() -> ()` (thus, an `Effect<()>`),

Here's a very basic, very obvious implementation of `Effect<Value>`:

```swift
struct Effect<Value> {
    private let call: () -> Value
    init(_ call: @escaping () -> Value) {
        self.call = call
    }
    
    func execute() -> Value {
        return call()
    }
}
```

Of course calling `execute` on the effect will not be pure, and it's something we'd like to do in a protected context. But `Effect<Value>` has a monad representation, that can be implemented in the following way:

```swift
extension Effect {
    static func pure(_ value: Value) -> Effect {
        return Effect.init { value }
    }

    func joined <A> () -> Effect<A> where Value == Effect<A> {
        return Effect<A>.init {
            self.execute().execute()
        }
    }
}
```

Within the `joined` function we don't call the `execute` method, so no effect is executed: we literally just return a new effect, the computation is pure. But `joined` will *simplify* a nested effect, with the macroscopic result of an execution, because it would go from `Effect<Effect<Value>>` to `Effect<Value>`.

At this point I think we understood fairly well why the abstract structure called "monad" can be useful in effectful computation; monads show **in mathematics** the equivalence of a particular *nested* structure to an *non-nested* one (given some laws are respected). Then we map this *nesting* into generic types et voilà: we get effectful computations that, within a certain context, magically become pure. I'm not sure I can explain the idea any better than that: this is what a monad is about, this is why it sounds like *monoid*, and this is why I think that *monad* is an appropriate word for representing this concept: it's like "multiplication", you just learn it and move on. So let's move on.

## How can I hold all these monads?

In this piece I'm not particularly interested in listing a bunch of useful monads in current use: there aren't many, and they mostly reflect the original [Eugenio Moggi's dissertation](https://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf) about using monads to represent effectful computations. You can find many articles about useful monads on the web, [here's one](http://adit.io/posts/2013-06-10-three-useful-monads.html) for example. Let me just produce another one here, that's going to be useful for a practical example, the `State<Environment,Value>` monad:

```swift
struct State<Environment,Value> {
    private let call: (Environment) -> (Environment, Value)
    init(_ call: @escaping (Environment) -> (Environment, Value)) {
        self.call = call
    }

    func run(_ environment: Environment) -> (Environment, Value) {
        return call(environment)
    }
}
```

`State<Environment,Value>` encapsulates a function of type `(Environment) -> (Environment, Value)`: this shape of function is useful if we want to produce some value given a certain contex (the `Environment` parameter) and also want to *modify* the context in some way. Now, `State<Environment,Value>` is a monad and we can provide the two defining functions for it:

```swift
extension State {
    static func pure(_ value: Value) -> State {
        return State.init { e in (e, value) }
    }

    func joined <A> () -> State<Environment, A> where Value == State<Environment, A> {
        return State<Environment, A>.init { m in
            let (m1,sub) = self.run(m)
            let (m2,p) = sub.run(m1)
            return (m2,p)
        }
    }
}
```

Notice that `State<Environment,Value>` is monadic in the `Value` parameter: that's the parameter passed to pure, and the nesting solved by `joined` is for a `State<Environment,State<Environment,Value>>` type, where the `Environment` **must be the same**, and the nesting is about putting another `State<Environment,Value>` in place of the `Value` of the first.

For example, we could have a list of objects of type `User` as the environment, and we'd want to represent a query on that list that retrieves a user and **deletes it** from the list, thus modifying the list itself. We're tempted to represent this operation with `State<[User],User>` but we're forced to allow the possibility that a `User` for a particular query might not be there, so a more appropriate representation would be `State<[User],Optional<User>>`. This is interesting because it's a kind of nesting of monads, but a different one: we're nesting the *optional* monad within the *state* monad. This **cannot** be joined, and this is one of the reasons why you often read that *monads don't compose*. But if we treated `State<[User],Optional<User>>` as a whole monad in itself (like we manufactured a new one), then it turns out that we could somehow *join* this: its nested representation would be something like `State<[User],Optional<State<[User],Optional<User>>>>`, which is hard to read, but kind of simple to understand: suppose that we wanted to perform a query on `[User]` that required information from another `User`; we do the first query, get the `User` (maybe, for it could be `nil`) then perform the second one with some information from the first. But of course we'd like to represent this with a single instance of `State<[User],Optional<User>>` that we can simply `run`. Is there a way to write a `joined` function that worked this way? Of course there is, and let's call it `joinedT` to not confuse it:

```swift
extension State {
    func joinedT <A> () -> State<Environment, Optional<A>> where Value == Optional<State<Environment, Optional<A>>> {

        return self
            .map { value -> State<Environment, Optional<A>> in
                switch value {
                case let .some(innerValue):
                    return innerValue
                case .none:
                    return .pure(nil)
                }
            }
            .joined()
    }
}
```

We also needed to add a `map` function to `State` to make it work but it's not a big deal; as already said, every monad is also a functor (or *mappable* if you prefer):

```swift
extension State {
    func map <A> (_ transform: @escaping (Value) -> A) -> State<Environment, A> {
        return State<Environment, A>.init { e in
            let (newE, p) = self.run(e)
            return (newE, transform(p))
        }
    }
}
```

Now, there are a few interesting things about the `joinedT` function, and here's the two most relevant for me:

- we took advantage of the knowledge of the particular structure of `Optional<Wrapped>` to make it work (we're switching over its cases);
- the only methods that we call on the root instance of the state monad are `map`, `pure` and `joined`.

The first point means that to do this particular operation we need to know the structure of the *internal* type, and this is kind of expected; but the second point is crucial: *we don't care* about the fact that the external type is a state monad, we only care about the fact that **it's a monad**. Here's the same implementation of `joinedT` for `Effect<Optional<Value>>` (providing `map` for `Effect<Value>`):

```swift
extension Effect {
    func map <A> (_ transform: @escaping (Value) -> A) -> Effect<A> {
        return Effect<A>.init {
            transform(self.execute())
        }
    }

    func joinedT <A> () -> Effect<Optional<A>> where Value == Optional<Effect<Optional<A>>> {
        return self
            .map { value -> Effect<Optional<A>> in
                switch value {
                case let .some(innerValue):
                    return innerValue
                case .none:
                    return .pure(nil)
                }
            }
            .joined()
    }
}
```

The implementation of `joinedT` is **identical**, save for the different types on the function signature. Again, we only care about the fact the `Effect<Value>` is a monad.

Here's the implementation of `joinedT` for `Array<Optional<Element>>` (providing a `pure` function for `Array`):

```swift
extension Array {
    static func pure(_ value: Element) -> [Element] {
        return [value]
    }

    func joinedT <A> () -> FlattenBidirectionalCollection<[[Optional<A>]]> where Element == Optional<Array<Optional<A>>> {
        return self
            .map { value -> Array<Optional<A>> in
                switch value {
                case let .some(innerValue):
                    return innerValue
                case .none:
                    return .pure(nil)
                }
            }
            .joined()
    }
}
```

The type is a little different here, because `joined` on a array returns a `FlattenBidirectionalCollection`, but the type could be made conform to the others with a different overload of `joined`. We're still only calling `map`, `pure` and `joined` on the base type, which simply requires that the base type be a monad.

To mix the things up a bit, here's the implementation of `joinedT` for `Effect<Array<Value>>`:

```swift
extension Effect {
    func joinedT <A> () -> Effect<Array<A>> where Value == Array<Effect<Array<A>>> {
        return self
            .map { value -> Effect<Array<A>> in
                value.reduce(.pure([])) { accumulation, element in
                    accumulation
                        .map { acc in element
                            .map { ele in
                                acc + ele
                            }
                        }
                        .joined()
                }
            }
            .joined()
    }
}
```

The implementation is different because now the internal type is an array, and we need to take into account its particular structure: but on the base type, `Effect<Value>`, we're still only calling `map`, `pure` and `joined`.

What about `pure`? Does it compose? Yes, and trivially:

```swift
extension Effect {
    static func pureT <A> (_ value: A) -> Effect<Array<A>> where Value == Array<Effect<Array<A>>> {
        return .pure(.pure(value))
    }
}
```

We just call `pure` within `pure`. We can actually do the same with `map`, producing `mapT`, also trivially:

```swift
extension Effect {
    func mapT <A,B> (_ transform: @escaping (A) -> B) -> Effect<Optional<B>> where Value == Optional<A> {
        return map { $0.map(transform) }
    }
}
```

I could go on indefinitely, but I think I made my point: it's true that monads don't strictly compose, but we can operate with different nested monads like we operated on simple monads, with functions that work exactly like those of the latter. As we saw, the only requirement at the implementation level is that the base type has the `pure`, `map` and `joined` methods. This sounds like a *protocol extension* to me!

If we had a `Monad` protocol, that had an `associatedtype` representing the *monadic* parameter (let's call it `ParameterType`), and declared the `pure`, `map` and `joined` methods, we could extend the protocol with a constraint on the `ParameterType` to be some other type, and we would take into consideration the particular structure of the latter, considering that we'll simply need to call `pure`, `map` and `joined` on the base monad.

Unfortunately, we cannot do this in Swift, and this is heart of the matter.

## Can we go higher?

As an exercise, try and write a `Monad` protocol: you can easily add the `pure` function, but when you go into `joined`, something strange happens. The return type of `joined` must be `Self` somehow, but it's a little different from actual `Self`, because it should have a different associated type:

```swift
protocol Monad {
    associatedtype ParameterType

    static func pure(_ value: ParameterType) -> Self

    /// this won't compile: Self<A> is a meaningless term
    func joined <A> () -> Self<A> where ParameterType == Self<A>
}
```

We would like to express the idea that the return type must be of the same concrete type of the instance we call `joined` on, but with a different generic parameter (or, in other words, the `associatedtype` should be different). But we can't do this!

The `kind` of a type represents the *shape* of its constructor; for example, a normal generic type like `Optional<Wrapped>` has a kind represented by the expression `* -> *`: this means that we can construct an optional type by providing some concrete type (the input `*`) and thus forming another concrete type (the output `*`). In the case of `Optional<Wrapped>`, if we provided `Int` as the *input type* we would get `Optional<Int>`, a concrete type, that's not generic anymore, as the *output type*. So, a particular generic type like `Optional<Wrapped>` has kind `* -> *`. In general, a protocol with an `associatedtype` represents a type of the kind `* -> *` because it takes some type (the one associated to it) and produces a concrete type represented by `Self`. What's the kind of a `Monad`? It's actually `* -> * -> *`, which means that there's another input type involved in the process of defining the final concrete type: this is the *base* type, that could be any type with a generic parameter. That's what HKT means: the kind `* -> * -> *` is **higher** than `* -> *`

I guess the case might be clearer if we considered a possible example, in pseudo-Swift:

```swift
/// pseudo-Swift
protocol Monad {
    basetype Self<_>
    associatedtype ParameterType
    
    static func pure(_ value: ParameterType) -> Self<ParameterType>
    
    func joined <A> () -> Self<A> where ParameterType == Self<A>
}
```

The declaration `basetype Self<_>` means that the type has a generic parameter, and we want to abstract over the `Self<_>` type constructor, by directly referring to it. A possible extension for `Optional<Wrapped>` would be like the following:

```swift
/// pseudo-Swift
extension Optional: Monad {
    typealias Self<_> = Optional<_>
    typealias ParameterType = Wrapped
    
    static func pure(_ value: Wrapped) -> Optional<Wrapped> {
        /// some code
    }
    
    func joined <A> () -> Optional<A> where Wrapped == Optional<A> {
        /// some code
    }
}
```

This is just an example - I'm not proposing this particular syntax - but I think it shows what would practically mean to represent a type constructor of kind `* -> * -> *`.

It's not possible to represent this kind of abstraction in Swift, and it's a shame: as we saw with the nested monads example, we could leverage the semantic power given by combining monads without having to specify the base type, by simply extending the `Monad` protocol with different implementations of `pureT` and `joinedT` based on the concrete associated type, like for example in the following (the last pseudo-Swift bit for now):

```swift
/// pseudo-Swift
extension Monad {
    static func pureT <A> (_ value: A) -> Self<Optional<A>> where Value == Optional<Self<Optional<A>>> {
        return .pure(.pure(value))
    }

    func joinedT <A> () -> Self<Optional<A>> where Value == Optional<Self<Optional<A>>> {
        return self
            .map { value -> Self<Optional<A>> in
                switch value {
                case let .some(innerValue):
                    return innerValue
                case .none:
                    return .pure(nil)
                }
            }
            .joined()
    }
}
```

This code would be valid for any monad with a nested optional.

I'd like to make another practical example to show the power of combining monads, by introducing another couple of useful ones:

- `Future<Value>`: it's used to model the effect related to producing a value at some point in the future or, in other words, manipulating a value that's not yet available; where `Effect<Value>` is executed with a simple `execute` method that returns a value immediately, the `Future<Value>` effect is executed by passing a *handler* function of type `(Value) -> ()`, that will be called once the value is available: in this sense, `Future<Value>` models the concept of [continuation](https://en.wikipedia.org/wiki/Continuation) or asynchronous calls in general.
- `Result<Failure,Value>`: it's used to model a computation that can fail, with some additional information attached to the failure, represented by the `Failure` type; it works similarly to `Optional<Wrapped>`, but we get an instance of `Failure` instead of just `nil`; Swift natively represents this with the concept of *throwing function*, but `Result<Failure,Value>` is much more powerful for a number of reasons:
    - it's a nominal type, so it can have methods and produce valid instances;
    - it carries information about the type of the error produced,  while still capable of being made adequately ergonomic with an `AnyError` container, the possibility to map the `Failure` part and many compositional utilities, for example when dealing with many failable operations at the same time;
    - it's intrinsically more composable: for example, it can be passed as the input to a function, with no friction.

Thus, `Future<Value>` makes for asynchronous calls, like HTTP requests for example, and they can usually fail to produce a valid `Value`, by providing some error instead: this seems the perfect case for nesting a monad into another, because we can represent the possibility of failure with the `Result<Failure,Value>` monad, producing finally a `Future<Result<Failure,Value>>` type. For this type, everything we said about `pureT` and `joinedT` is still valid, and this case could be even more relevant, because it's frequent that we need to *chain* together more asynchronous calls, where the latter depends on the result of the former.

Again, I'm not interested in talking about the single monads: they're useful per se, and we don't need the notion of HKT to use them; but their composition is exponentially more powerful, and without HKTs the only option is to generate the `pureT` and `joinedT` functions for *every possible combination* of monads; and if we add a new monad in our code base at a certain point in time, we need to generate a lot of new code just for that, a solution that obviously doesn't scale. I could produce more examples, but there are many other types of effects that can be worked with within a monadic context, some probably not yet discovered: but without a sense of HKTs in the language these entities remain isolated, and cannot be composed without tedious repetition of code: loads of code generation help, but they're usually not the solution to the problem.

## A deep nest

The problem is not just horizontal (more monads' combinations) but also vertical (deeper nesting). For example, what if our asynchronous call produced a list of values, and from each value we want to make another asynchronous call that produces a list of values, finally bundling all the calls together into a single list? We start from a type like

`Future<Result<Failure,Array<StartingValue>>>`

and we've got a function of type

`(StartingValue) -> Future<Result<Failure,Array<EndingValue>>>`,

where there's one more level of nesting. We'd like to go from some monster like

`Future<Result<Failure,Array<Future<Result<Failure,Array<Value>>>>>>`

to the joined version

`Future<Result<Failure,Array<Value>>>`

and it turns out the if we can define `pureT` and `joinedT`, we'll also be able to define `pureTT` and `joinedTT`. The former is simple:

```swift
extension Future {
    static func pureTT <E, A> (_ value: A) -> Future<Result<E, Array<A>>> where Value == Result<E, Array<A>> {
        return .pureT(.pure(value))
    }
}
```

notice that on the base type we only call `pureT`, which means that we only need that the base type is a monad to make this work, because we were able to extend our hypothetical `Monad` protocol  with the `pureT` function. `joinedTT` is where the true magic happens: if you recall `joinedT` on `Effect<Array<Value>>`, its implementation didn't care about the fact that the base type was an `Effect`, and only used `pure`, `map` and `joined` on it, while taking into account the particular structure of `Array`, that is, the internal type; now, in the case of `Future<Result<E, Array<Value>>>`, again the most internal type is an `Array` and again we were able to define `pureT`, `mapT` and `joinedT` on `Future<Result<E, Value>>`, so in theory this implementation of `joinedTT` should be like `joinedT` for `Effect<Array<Value>>`, where we simply swap `pure`, `map` and `joined` with `pureT`, `mapT` and `joinedT`. Impressively, that's exactly the case:

```swift
extension Future {
    func joinedTT <E, A> () -> Future<Result<E, Array<A>>> where Value == Result<E, Array< Future<Result<E, Array<A>>>>> {
        return self
            .mapT { value -> Future<Result<E, Array<A>>> in
                value.reduce(.pureT([])) { accumulation, element in
                    accumulation
                        .mapT { acc in
                            element.mapT { ele in
                                acc + ele
                            }
                        }
                        .joinedT()
                }
            }
            .joinedT()
    }
}
```

The types change, but the *shape* of the whole thing is the same. That would work also for `joinedTTT`, `joinedTTTT` and so on: the point is, at any level of nesting for a type like

`Base<Nested1<Nested2<Nested3<...>>>>`

to make the `_T` functions work we require that `Base` is a monad, whatever monad it is, and we need to know the internal structure of `Nested1`, `Nested2` and so on, because we need to be able to write `joinedT`, `joinedTT` and so on. Notice that we're not abstracting the types *in the middle*, but we should do that otherwise we'd still need to generate a lot of code, even with HKTs. Now, there's is actually a way to do that, that would allow us to abstract over even the most internal type: it would require to be able to define a type as [`Traversable`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Traversable.html), something that requires again HKTs. But I don't want to add too many eggs to this basket: I think I already made a clear example for the usefulness of higher-kinded typing when using monads.

## Conclusion

The goal of this article was twofold: introducing monads from a slightly different perspective than usual, and show a case for the addition of HKTs to Swift (or any language with generics, really). Unfortunately, this endeavor requires a lot of words and code to be even vaguely worth of consideration, and this is a big issue with many functional programming concepts: once you live with them, they become the norm and actually shape your mind in approaching the problems in a certain way, but when you've just started to (or you want to be convinced to) consider them, there seems to be no real "perfect" strategy, because:

- the **inductive** approach, based on practical examples, doesn't work because examples, even if well structured and abundant, are never really enough, due to their very nature: there are always alternatives, that are often considered equivalent even if they're not necessarily equally valid, and specific cases are often too specific, and hard to expand into more generic ones that may include each particular problem;
- the **deductive** approach, based on theory, is often too obscure and alienating for most programmers, because, at least as I was able to observe in my experience, the programming field is usually not very fond of the theoretical way in general, and of the approaches based on analyzing problems purely in the abstract, only considering generic models.

I guess it's a truism that the right path is in the middle: honestly I'm not sure about that, but what I'm pretty sure about is that when approaching functional programming for the first time, there's a certain *leap of faith* involved, a trust that things will eventually make sense.

But there's one thing I can say for sure about the whole functional programming approach, and what you would eventually find there: functional programming is critically concerned with being as much as possible **precise** and **correct**. *Failing* (crashing) is not considered a viable option in almost all cases, and the few ones where an assertion is inevitable are treated as *exceptional* cases (it sounds like "exception", in fact) that must be **very, very carefully controlled and isolated**, and controlling effects, for example through monads, plays a huge role there. I'm not saying that functional programming is *the only* approach that's concerned with preciseness and correctness, but it certainly is one of the most. I hope I was able to shed some light on why, once upon a time, someone even considered applying mathematical concepts like monads to programming, and on why these structure are useful in solving practical programming problems: now it's up to the reader to consider taking this leap.

## Extra theory: HKTs emulation

HKTs are a **major** feature of a language, and even if the Apple core Swift team has shown a tepid interest in adding them to Swift, this is not going to happen in a short time (if ever), both for the complexity of the feature, and because there's simply a lot of much more important stuff to be done first. Are we left dead in the water, then? Well, mostly, but not completely: code generation can help, for example, when compositing monads, but it doesn't really scale (I had to stop at `_TT` in [FunctionalKit](https://github.com/facile-it/FunctionalKit) because the code size and the compilation time would have been unbearable with 3 levels of nesting) and it must be redone every time a new monadic type is added. But there's a different option, that is, *emulating* HKTs.

With a few tricks, we are actually able to write a *container type* parametrized over a value **and** a type constructor (actually, its *tag*). We can then define our monad methods in terms of this container, from which we can extract the original "containers" (like `Array`, `Optional`, `Future`) with the correct parameter applied. This technique is based on an [article](http://ocamllabs.io/higher/lightweight-higher-kinded-polymorphism.pdf) by Jeremy Yallop and Leo White, which is an interesting read but I think that exploring the many implementations of it, discovered by just googling "simulating higher kinded types", can be enough to get a general idea.

Instead of explaining the whole thing again in Swift, I'll refer to a couple of resources:

- this [wonderful gist](https://gist.github.com/anandabits/f12a77c49fc002cf68a5f1f62a0ac9c4) from [Matthew Johnson](https://github.com/anandabits) that explains the basics of the technique pretty well;
- this [library](https://github.com/inamiy/HigherKindSwift) from [Yasuhiro Inami](https://github.com/inamiy) that contains a possible implementation, including some concrete extensions of Swift types.

The main idea here is to wrap types into a container for which we can actually define the various `map`, `joined` et cetera in an abstract way via a proper procotol, and then extract the base types with a forced unwrap that we're sure is going to work thanks to the constraints posed on the types. The idea seems solid, but I'm yet to see Swift production code that uses HKTs defined that way, and I've not explored the concept myself enough to have formed a complete picture of it's feasibility in my day-to-day work: still, it looks definitely promising.

In the next article I'm going to talk about [Profunctor Optics](https://arxiv.org/pdf/1703.10857.pdf), something that **heavily** needs HKTs to be defined - code generation is not going to help there - and I intend to explore HKTs emulation for that particular application.

Until next time.