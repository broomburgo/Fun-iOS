+++
title = "Algebraic data types for contextual and conditional computation"
date = 2018-03-19T15:00:01+01:00
type = "post"
draft = false
+++

An [algebraic data type](https://en.wikipedia.org/wiki/Algebraic_data_type) (**ADT** from now on) is a type (`class`, `struct`, `enum`, whatever the language allows us) that has no intrinsic semantics, and only exists for combining two or more other types in a specific way.

For example, in Swift the *tuple* `(Int,String)` is an ADT, in the sense that the only information it conveys is the fact that one instance of it will contain one instance of `Int` **and** one instance of `String`. The reason why it's called *algebraic* can be object of a fun exploration, but I don't want to talk about it right now: if you're interested I suggest reading [this excellent article](http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/) and subscribing to the wonderful [PointFree](https://www.pointfree.co) series - episode 4 is about ADTs, and there probably will be many more on the topic.

In this article I'm going to talk about 2 ADTs, and in which cases they can be useful for real-world computational problems. I'm also going to suggest a specific naming convention for these  types, based on [category theory](https://www.youtube.com/watch?v=IBeceQHz2x8) (if you're interested I strongly suggest following the resource I just linked, but let's not digress).

## Products

A **product type** is the combination of 2 types. We could define a product of *more than 2* types but it's easier to treat and reason about the case with 2, knowing that we can always decompose a bigger combination in smaller pairs.

A nice way to define the characteristics of a type is in terms of what kind on instances I'm expecting for it: for example, `Bool` is a type with exactly *2 possible instances*, `true` and `false`: in fact, when treating types algebraically, `Bool` is usually called `Two`. `Int` is a type that has, on my computer, `2^64` possible instances, which can be considered, in practice, *infinite*. The following `enum`:

```swift
enum Direction {
    case north
    case south
    case west
    case east
}
```

has exactly 4 instances, so it would called `Four`. But also the following:

```swift
enum Element {
    case earth
    case wind
    case fire
    case ice
}
```

has 4 instances, so should this also be `Four`? Yes, because from an algebraic standpoint, they are *isomorphic*: each instance of the first corresponds to a specific instance of the second, and viceversa. When talking about the algebra of types, we are interested about their *shape*, and how different shapes are related to each other and compose: domain-specific names like `Direction` and `Element` are irrelevant.

Being a combination of two types means, for a product type, that every instance of it contains one instance of the first and one instance of the second, so the number of its possible instances is the number of possible instances of the first **times** the number of possible instance of the second, therefore we call it **product**. In set theory, a product set would be the *cartesian product* between two sets, that is, the set of all possible *pairs* of elements from the two sets: in fact, a usual way to define the product type in computation is with the `Pair` class/struct. But I prefer to be more explicit and call this type `Product`. Product of what? *Any two other types*, so it's going to be generic of course:

```swift
struct Product<A,B> {
    let first: A
    let second: B
}
```

A product is usually defined with the `first` and `second` properties, holding the instances of the types with which the product is formed. But I prefer a more category-theoretical way of defining it, that is, in terms of the *functions* from the product itself to its components:

```swift
func first<A,B>(_ product: Product<A,B>) -> A {
    return product.first
}

func second<A,B>(_ product: Product<A,B>) -> B {
    return product.second
}
```

The functions `first` and `second` have type, respectively, `(Product<A,B>) -> A` and `(Product<A,B>) -> B`: this is a universal way of defining what a product is, that is, if these functions exist for a type, then that type is a product (from a category-theoretical standpoint it's a little more complex than that, but it's sufficient to say for now that something is a product if those functions are sufficient to define it, without any additional information involved or computational burden).

Some of you may have noticed that a product is really a tuple of  two elements, so `Product<A,B>` could actually be defined as a `typealias`, redefining also the `first` and `second` functions, like the following:

```swift
typealias Product<A,B> = (A,B)

func first<A,B>(_ product: Product<A,B>) -> A {
    return product.0
}

func second<A,B>(_ product: Product<A,B>) -> B {
    return product.1
}
```

The reason why I prefer to encapsulate a product inside its own `struct` is that this way we can extend it with methods (in Swift we can only add methods to "nominal" types).

> A quick digression. Using free functions is *a lot* more powerful than writing extensions in Swift, due to the limitations of its current type system, but I'm going to stick to methods when possible for their familiarity: also, methods give us a *scope* to work with, and Xcode autocompletion when writing a dot `.` is good enough to justify the burden of extending types instead of using something that's so much simpler to handle and compose like free functions. This also means that I'm not going to use the `first` and `second` free functions, and will prefer the direct call of `.first` and `.second` properties.

So, if I give you a product it means that I'm giving you *two things* in a generic way, without any additional burden related to domain-specific constructs. Ever wanted to return two things from a function instead of one? The product is the way.

## Contextual computation

A classic use case for passing around two things instead of one is when we a have a *main* piece of information paired with some additional *context*. Suppose for example we want to transform a   set of goods into something suitable for the user who's currently in session, ordered by the user's preference; we could write a function like this:

```swift
func getPreferredGoods(from set: Set<Item>) -> List<Item> {
    /// implementation by using a "global" user
}
```

We're probably going to grab the user currently in session (from some global instance) and use the user's preferences to first filter the `Set<Item>`, and then order it based on the order of preferences. To make this function decently isolated (and pure) we could easily improve this by passing also the `User` in, but instead of passing another parameter in the function, let's use a `Product` instead:

```swift
func getPreferredGoods(from product: Product<User,Set<Item>>) -> List<Item> {
    /// implementation by using the user contained in the product
}
```

Conventionally, the "main" part of the product is the `second` part, while the context is the `first`. The improvement we achieve by using an explicit product type is when we decide that we're also going to filter the list of goods by availability per age, considering again the user in session:

```swift
func getAgeAvailable(from items: Product<User,List<Item>>) -> List<Item> {
    /// implementation by using the user contained in the product
}
```

Now we have two functions that are both of the same shape, that is, `Product<Context,A> -> B`, and we'd like to compose them. In our specific example, the `List<Item>` returned from the first function is the input of the second, but it's paired again with `User`. We could in theory return again a `Product` by it would be cumbersome because each time we would be simply pairing the result again with the `User`, something that would be discarded anyway by the actual caller of the function.

To solve this, in a completely generic way, we can add a method to `Product<A,B>` that accepts a function in the shape `Product<A,B> -> C` and returns a new `Product<A,C>`, let's call it `extend`:

```swift
extension Product {
    func extend <C> (_ transform: (Product<A,B>) -> C) -> Product<A,C> {
        return Product<A,C>.init(
            first: first,
            second: transform(self))
    }
}
```

As you can see, this simply gets the new value and pairs it again with the context. To stress the fact that the `second` value is our *main* value let's add a computed property `extract` that simply returns it:

```swift
extension Product {
    var extract: B {
        return second
    }
}
```

Now, given that we have our initial product, we can compose the functions like the following:

```swift
let initial: Product<User,Set<Item>> = ...

let items = getPreferredGoods(initial)
    .extend(getAgeAvailable)
    .extract
```

Now, in case we also need to add another transformation - that requires the `User` as context - to this "pipeline" we simply need define our function in an isolated way, and then `extend` again:

```swift
func getOrder(considering items: Product<User,List<Item>>) -> Order {
    /// implementation
}

let initial: Product<User,Set<Item>> = ...

let order = getPreferredGoods(initial)
    .extend(getAgeAvailable)
    .extend(getOrder)
    .extract
```

Let's consider a different case: we want to compute the price for the order in the same pipeline. This a slightly different computation, however, because we really don't care about an `User`, so our isolated function would really look like this:

```swift
func getPrice(from order: Order) -> Price {
    /// implementation
}
```

We can easily plug this function into our pipeline by adding another generic method to `Product<A,B>`:

```swift
extension Product {
    func map <C> (_ transform: (B) -> C) -> Product<A,C> {
        return Product<A,C>.init(
            first: first,
            second: transform(second))
    }
}
```

This is called `.map`, like `Array.map`, `Optional.map` and so on, and does the same thing: it transform the *contained* value without affecting the *container*. Finally:

```swift
let price = getPreferredGoods(initial)
    .extend(getAgeAvailable)
    .extend(getOrder)
    .map(getPrice)
    .extract
```

## Doubling on information

Another use case for using products to represent our states is related to functions that return some main value coupled with additional information, like a **log**: a function might compute something and return it, while also informing the user about the process, with, usually, some text. We could, of course, just log in console, but there are cases where we'd probably interested in catching this log and **choose later** what to do with it: for example, saving it into a file, sending it to some web services to collect data and so on.

As before, we'd like to compose functions in this shape:

```swift
func computeWithLog <A,B> (_ value: A) -> Product<Log,B> {
    /// implementation
}
```

and produce at the end a value of type `Product<Log,Something>`. Our "context" in this case is not used within the function, but it's supposed to be useful for the caller. We can try and compose functions in this shape by adding a new generic method to `Product<A,B>`, but for this use case we incur in some specific challenges, as we can see from the following:

```swift
extension Product {
    func wrongCompose <C> (_ transform: (B) -> Product<A,C>) -> Product<A,C> {
        return transform(extract) /// this is wrong
    }
}
```

The problem with this is that we're essentially **forgetting the previous context**: the returned product is completely produced by the `transform` function, and the previous context is lost. What if we simply `map` it? Let's try:

```swift
let initial: Product<Log,A> = ///

func computeWithLog (_ value: A) -> Product<Log,B> { /// }

let final = initial.map(compute) /// this has type Product<Log,Product<Log,B>>
```

As you can see in the code, the type of `final` is `Product<Log,Product<Log,B>>`, definitely not what we wanted: it's nested products, and we'd like to *join* them for good, into a single `Product<Log,B>`. To do this we need a way to generically compose instances of type `Log`: there's **a lot** to be said about this topic, but it's not in the scope of this article to discuss about types with a generic composition operation. For now let's simply use a type that we're sure composes easily and obviously, an `Array`: let's just use `[Log]` instead of `Log`.

We can actually *flatten* an instance of type `Product<[Log],Product<[Log],B>>` very easily, but this is going to be a case where, due to the limitations of Swift generics system, we can really only do this with a free function:

```swift
func flatten <A,B> (_ nested: Product<A,Product<A,B>>) -> Product<A,B> where A: RangeReplaceableCollection {
    let context1 = nested.first
    let context2 = nested.extract.first
    return Product<A,B>.init(
        first: context1 + context2,
        second: nested.extract.extract)
}
```

This way we're not losing any information by staying completely generic. Notice that we're not explicitly saying that `A` is an `Array`: we only need to constrain `A` to be a `RangeReplaceableCollection`, so we can use the `+` operator to concatenate the two collections.

So, we can transform a `Product<Log,A>` with a function of type `(A) -> Product<Log,B>` by first *mapping* the product, and the *flattening* the result. To put things together, let's just define a `flatMap` method (see why it's called *flatMap*?):

```swift
extension Product where A: RangeReplaceableCollection {
    func flatMap <C> (_ transform: (B) -> Product<A,C>) -> Product<A,C> {
        return flatten(map(transform))
    }
}
```
 
`Product.flatMap` is like `Array.flatMap` and `Optional.flatMap`: transforms a container with a function that turns the contained value into another container, then joins the nested containers into one. Finally, to explicitly "read" the `Log`s, along with the computed value, we can add a simple computed property that returns a tuple:

```swift
extension Product {
    var read: (A,B) {
        return (first,second)
    }
}
```

The transformation pipeline that we're expecting here is something like the following:

```swift
let initial: Product<[Log],A> = ///

let (logs, final) = initial
    .flatMap(computeWithLog1)
    .flatMap(computeWithLog2)
    .flatMap(computeWithLog3)
    .read
```

What `Log` we're going to have for our `inital` value? No log at all, so we need a generic way to encapsulate some value into `Product<[Log],A>` with no log attached; let's add a `.pure` static method:

```swift
extension Product where A: RangeReplaceableCollection {
    static func pure(_ value: B) -> Product<A,B> {
        return Product<A,B>.init(first: A.init(), second: value)
    }
}
```

The `RangeReplaceableCollection` protocol allows us to generate instances (of a type conforming to it) with no values, thus constructing *empty* instances.

The *log* example is classic for this kind of product composition, but there are many other cases where this idea of composition based on *flatMap* turns out to be useful: the idea is that we want to *attach* some other value to our main computed value, and instances of the attached value must be composable in a generic way; here's a few examples:

- discarded information that we want to keep around;
- executable functions that perform side effects and return `Void`;
- a state that must be updated (es. a session token or a date);
- some quantity that needs to be accumulated at every step;

Of course we could obtain all this functionality and more with a bunch of **ad-hoc** types with made-up names that are supposed to be sensible (there's even literature on how to chose names for ad-hoc classes and methods), but the good thing about `Product<A,B>` is that it's completely generic, conveys a specific meaning very clearly, and offers a lot of composition utilities (many more than I just shown).

## Coproducts

When approaching things in a category-theoretical way we essentially double our abstraction powers, because a key concept in category theory is *duality*: by inverting the relationships, I get something new.

I initially defined a product with a struct, and then shown a representation with the `first` and `second` functions, that is, functions in this shape:

```swift
let first: (Product<A,B>) -> A
let second: (Product<A,B>) -> B
```

Starting from the functions this time around we can come up with a *dual* representation - in which everything is *co-something* - by simply inverting the arrows:

```swift
let coFirst: (A) -> Coproduct<A,B>
let coSecond: (B) -> Coproduct<A,B>
```

`Coproduct` needs to be a type independently constructible from `A` and from `B`, and because `A` and `B` are generic, there isn't going to be any information about `B` if the type is constructed with `A`, and viceversa (that's for saying that `Coproduct` is not going to pull the instance without which is was constructed out of the hat). This type could be represented in many ways, but luckily in Swift we have a structure that's perfect for coproducts: an `enum` with associated values.

```swift
enum Coproduct<A,B> {
    case left(A)
    case right(B)
}
```

The functions that produce a `Coproduct` are called `left` and `right` to suggest an alternative choice (instead of `first` and `second` that suggest the presence of both), and their implementation could be the following:

```swift
func left <A,B> (_ value: A) -> Coproduct<A,B> {
    return Coproduct<A,B>.left(value)
}

func right <A,B> (_ value: B) -> Coproduct<A,B> {
    return Coproduct<A,B>.right(value)
}
```

> Unfortunately, it's not possible to use these functions freely in Swift without specifying the returned type, because in Swift a generic function cannot be specialized when calling it. But as for `Product<A,B>`, I'll prefer the usage of the `.left` and `.right` cases instead of the free functions, and cases must be scoped (ex. `Coproduct<Int,String>.left(42)`) to be used, removing the problem we have with free functions.

An instance of a coproduct contains information *either* for a case *or* the other, while a product contains *both*. Notice that associated values can have type `()` (typealiased to `Void`), for example `Coproduct<(),String>`; a type like this could be a little annoying because for calling the `.left` case we would need something like the following:

```swift
let x = Coproduct<(),String>.left(())
```

Swift explicitly requires an "instance" of `()`, which is of course the empty tuple `()` (the same symbol is used for the type and the instance). But, if we think about it, a `Coproduct<(),String>` is really a type that either contains a `String` or *nothing* (the empty tuple), and there already exist a type like that: I'm talking about `Optional<String>`. An `Optional` is a coproduct for which a case has no associated value, and it's used to represent some piece of information, or lack of thereof, and if we really need a coproduct from an optional we can always obtain it completely generically:

```swift
extension Optional {
    func toCoproduct() -> Coproduct<(),Wrapped> {
        switch self {
        case let .some(value):
            return .right(value)
        case .none:
            return .left(())
        }
    }
}
```

This just gave us an insight on how coproducts are commonly used as a computational tool.

## Conditional computation

A coproduct can be used to represent either something or something else. If a function returns a coproduct, it's expressing the idea that two possible things could have happened inside the function, with two possible outcomes. The classic use case for a coproduct is represented by functions that could return an *error*.

Consider a function with this shape:

```swift
enum OrderError: Error {
    case noPaymentMethod
    case noUserPermission
}

func process(order: Order) -> Coproduct<OrderError,OrderResult> {
    /// check if order is complete
}
```

Just by looking at the function signature we immediately see that an `OrderError` could be produced, describing the problem that might have arisen. This is similar to what `throws` expresses in idiomatic Swift, but there are several advantages in using a coproduct explicitly.

Firstly, by using a coproduct we are explicitly indicating the error type, while `throws` coalesces everything under the `Error` umbrella, requiring matching errors with `catch` that's not guaranteed to be exhaustive (while `switch` is).

But more importantly, `Coproduct<A,B>` easily composes and can be used in situations where `throws` is cumbersome or straight-out unusable. For example, consider an asynchronous version of the `process(order:)` function, that takes a callback:

```swift
/// Version 1: inconvenient and ugly
func process(order: Order, onComplete: (OrderError?, OrderResult?) -> ()) { }

/// Version 2: cumbersome and uninformative
func process(order: Order, onComplete: (() throws -> OrderResult) -> ()) { }

/// Version 3: perfect
func process(order: Order, onComplete: (Coproduct<OrderError,OrderResult>) -> ()) { }
```

- `Version 1` shows the classic approach, adopted in languages with a limited type system, and it's the worst of all 3: we manually need to check and unwrap both the error and the result, without any guarantee that either of them will be there, because the type system is not helping us.

- `Version 2` is better: the type system now is more explicit, and meaningless states (like no error **and** no result) cannot be represented, but we're getting a *function* as input to the callback, which means that to get the result we need to call it, and implementing `process(order:)` becomes more complex. Also, we're not getting any information about the error's specific type.

- `Version 3` is the best of both worlds: we're getting a value, over which we can pattern match, and we're being specific about the returned error.

By convention, the `.right` case is the one reserved to the *happy path*, where everything is fine (in fact, is *right*), and the `.left` case is used for when there's an error. To operate within a coproduct, whatever the branch is, we use composition methods like the ones we saw for products. Suppose for example we want to operate with the value contained in a coproduct without unwrapping it; we can easily transform it with a simple `map`:

```swift
extension Coproduct {
    func map <T> (_ transform: (B) -> T) -> Coproduct<A,T> {
        switch self {
        case let .left(value):
            return .left(value)
        case let .right(value):
            return .right(transform(value))
        }
    }
}
```

We're still following the convention that the `.right` value is the one we want to operate upon. Like with `Array`, `Optional` and `Product`, the `.map` method allows us to operate within a container without opening it: we're offloading the machinery to transform the contained value to the container itself.

What if the function transforming the contained value returned another `Coproduct` with the same kind of error? As we did with `Product`, we can `.flatMap` the whole thing, still assuming that the `.right` case is the good one.

```swift
extension Coproduct {
    func flatMap <T> (_ transform: (B) -> Coproduct<A,T>) -> Coproduct<A,T> {
        switch self {
        case let .left(value):
            return .left(value)
        case let .right(value):
            return transform(value)
        }
    }
}
```

Then, a series of functions in the shape `(A) -> Coproduct<B,C>` can be chained in a pipeline like the following:

```swift
let transform1: (A) -> Coproduct<SomeError,B>
let transform2: (B) -> Coproduct<SomeError,C>
let transform3: (C) -> D
let transform4: (D) -> Coproduct<SomeError,E>

let input: A

let output = transform1(input)
    .flatMap(transform2)
    .map(transform3)
    .flatMap(transform4)
```

To conclude this section, in the case we have different errors, we can - of course - have a `.mapError` function:

```swift
extension Coproduct {
    func mapError <T> (_ transform: (A) -> T) -> Coproduct<T,B> where T: Error {
        switch self {
        case let .left(value):
            return .left(transform(value))
        case let .right(value):
            return .right(value)
        }
    }
}
```

I think I made the point for using `Coproduct` for handling errors, let's move on.

## Railway programming

Another interesting use case for coproducts is the one when we don't have a *privileged* path, and we really want to handle both paths at the same time. The problem arises when reasoning in terms of pipelines, in which some information is passed from one stage to another, but we incur in the case where the pipeline is supposed to *split*. A classic (nowadays) transformation pipeline frequently used in iOS and app development in general is a chain of `Observable` operators, like those in [RxSwift](https://github.com/ReactiveX/RxSwift). Let's assume we retrive an `Int` from some `Payload`, and then we must split our pipeline based on the value of such `Int`:

```swift
let rxPayload: Observable<Payload> = ...

let rxOutcome = rxPayload
    .map { $0.getIntValue }
    ??? /// here we should have two lanes, one for intValue < 0 and one for intValue >= 0
```

One possibility here is to map our `Int` into a `Coproduct<Int,Int>` that has different meaning based on the `case`. Before going further, we need methods on `Coproduct<A,B>` that allow us to operate on the branches without assigning a particular "priority" to any of them:

```swift
extension Coproduct {
    func mapLeft <T> (_ transform: (A) -> T) -> Coproduct<T,B> {
        switch self {
        case let .left(value):
            return .left(transform(value))
        case let .right(value):
            return .right(value)
        }
    }

    func mapRight <T> (_ transform: (B) -> T) -> Coproduct<A,T> {
        switch self {
        case let .left(value):
            return .left(value)
        case let .right(value):
            return .right(transform(value))
        }
    }
}
```

We also need a way to *reconcile* the branches into a single value, that is, given a `Coproduct<A,B>`, we'd like to always be able to produce a single generic `T`: in a sense, given a coproduct, we'd like to *fold* it into something else, the same way we fold a sequence with a `reduce` method, transforming an entire list into a single value. We need a new method, let's call it `fold`:

```swift
extension Coproduct {
        func fold <T> (onLeft: (A) -> T, onRight: (B) -> T) -> T {
        switch self {
        case let .left(value):
            return onLeft(value)
        case let .right(value):
            return onRight(value)
        }
    }
}
```

The implementation is simple, but the implications are many: `fold` allows us to escape from the "world of coproducts", and go back to the mortal plane, in a completely generic and type-safe way. I consider the concept of *folding* at least as important as the idea of *mapping*.

To make things easier we'd like to extend `Observable` to easily `.map` on the `.left` and `.right` cases: unfortunately, Swift type system is not good enough to let us generically extend a generic type where the parameter is again generic. So we're going to write free functions (like we did with `flatten`) and add a `let` operator to `Observable` to permit extensibility with functions of type `(Observable) -> Observable`:

```swift
extension Observable {
    func `let` <A> (_ transform: (Observable) -> Observable<A>) -> Observable<A> {
        return transform(self)
    }
}
```

The idea is to add functions that operate on an `Observable<Coproduct<A,B>>` and allow to directly manipulate one of the branches; those functions can be chained in any order, because those on the `.left` branch are not going to interfere with those on the `.right`.

```swift
func mapLeft <A,B,C> (_ transform: @escaping (A) -> C) -> (Observable<Coproduct<A,B>>) -> Observable<Coproduct<C,B>> {
    return {
        $0.map {
            $0.mapLeft(transform)
        }
    }
}

func flatMapLeft <A,B,C> (_ transform: @escaping (A) -> Observable<C>) -> (Observable<Coproduct<A,B>>) -> Observable<Coproduct<C,B>> {
    return {
        $0.flatMap {
            $0.fold(
                onLeft: { transform($0).map(Coproduct.left) },
                onRight: { .just(.right($0)) })
        }
    }
}

func mapRight <A,B,C> (_ transform: @escaping (B) -> C) -> (Observable<Coproduct<A,B>>) -> Observable<Coproduct<A,C>> {
    return {
        $0.map {
            $0.mapRight(transform)
        }
    }
}

func flatMapRight <A,B,C> (_ transform: @escaping (B) -> Observable<C>) -> (Observable<Coproduct<A,B>>) -> Observable<Coproduct<A,C>> {
    return {
        $0.flatMap {
            $0.fold(
                onLeft: { .just(.left($0)) },
                onRight: { transform($0).map(Coproduct.right) })
        }
    }
}

func reconcile <A> (_ observable: Observable<Coproduct<A,A>>) -> Observable<A> {
    return observable.map {
        $0.fold(
            onLeft: { $0 },
            onRight: { $0 })
    }
}
```

We're using the methods we just defined for `Coproduct<A,B>` to add useful functions for operating directly on `Observable`: notice that we also added a `reconcile` function, that's useful when we have an observable of a coproduct, but the branches hold the same type. To proceed with our example, we could imagine an observable chain where, in a case, we're acquiring some information about the validity of our value via a HTTP request, while in another we're directly manipulating the input value:

```swift
let rxPayload: Observable<Payload> = ...
let requestValidity: (Int) -> Observable<Outcome> = ...

let rxOutcome: Observable<Outcome> = rxPayload
    .map { $0.getIntValue }
    .map { $0 < 0
        ? Coproduct.left($0)
        : Coproduct.right($0)
    }
    
    /// handle "left" branch
    .let(mapLeft { intValue in
        "Value \(intValue) is not valid"
    })
    .let(mapLeft(Outcome.init(string:))
    
    /// handle "right" branch
    .let(flatMapRight(requestValidity))
    
    /// reconcile to "Outcome"
    .let(reconcile)
```

Notice that the *left* and *right* subchains could be added in any order (even mixed) because they're completely independent. At the final stage, when the reconcile happens, the only thing that's required is that both branches contain the same type of value, in this case `Outcome`; this is how an *effectful* computation, that requires conditional branching, is kept **pure**: we branch, then we fold.

If the branches don't end up containing the same value, for example when we use a `Coproduct<ErrorType,ValueType>` where the left branch is an error, we can reenter the Swift world of imperative programming by defining a `.run()` method that's designed to *run the effect*. In the case of an error branch, our `.run()` method could be annotated with `throws`:

```swift
extension Coproduct where A: Error {
    func run() throws -> B {
        switch self {
        case let .left(error):
            throw error
        case let .right(value):
            return value
        }
    }
}
```

Going the opposite way, from a throwing function to a coproduct, is still possible but we need to erase the error type into an `AnyError`:

```swift
struct AnyError: Error {
    let get: Error
}

extension Coproduct where A == AnyError {
    static func from(throwing: () throws -> B) -> Coproduct<A,B> {
        do {
            return try .right(throwing())
        }
        catch let error {
            return .left(AnyError.init(get: error))
        }
    }
}
```

When we're in the "coproduct world" the single values are no longer accessible, unless we "force unwrap" them (asserting the existence of a specific branch): the case is similar with `Optional`, where we could force unwrap it with the `!` operator, and like with `Optional` we should **never** do that unless we're bound by a supernatural force (i.e., cocoa methods). The point of this whole article is that, for the majority of our program, we should operate within safe contexts, and then, at the *boundary*, run the effectful computations. This idea is highly related to the concepts expressed in the notorious [Boundaries](https://www.destroyallsoftware.com/talks/boundaries) talk by [Gary Bernhardt](https://twitter.com/garybernhardt): if you never watched it, you'd do a disservice to yourself if didn't watch it *right now*.

## Bonus Theory: a whole lot of jargon.

I purposely used the names `Product` and `Coproduct` to indicate the ADTs I talked about because they're not *familiar* in the context of software development. They're frequently used in the context of category theory, and I like them because they have a *precise* name that conveys clear meaning: a *product* of *A* and *B* is something that *contains both A and B*, and a *coproduct* is its *dual*, and being clear about an instance of duality between to types is, to me, extremely useful to unlock hidden properties that might not be apparent at a first look.

In line with the category-theoretical approach, we can define a product in terms of its *projections*, i.e., the functions that, given a product, yield one of its components:

```swift
let first: (Product<A,B>) -> A
let second: (Product<A,B>) -> B
``` 

These are of course the properties of a product if expressed as methods. An interesting way to write `Product<A,B>` is by using an operator to represent the idea:

```swift
Product<A,B> === A * B
```

The `*` operator indicates multiplication, as usual, but it's a multiplication *at the type level*. As stated above, the quintessential product is a *tuple* `(A,B)`, but as a "nominal" type, `Product<A,B>` is frequently called `Pair<A,B>`, and that's the first "divergence" from this essay and what's frequently found in literature, other articles or wikipedia.

To express the dual type, we invert the relationships, ending up with *injections*:

```swift
let left: (A) -> Coproduct<A,B>
let right: (B) -> Coproduct<A,B>
```

As we saw before, this is perfectly implemented by an `enum`. In **type theory** a coproduct is usually called *sum type*, and can be represent via the `+` operator:

```swift
Coproduct<A,B> === A + B
```

In set theory this is frequently referred as *tagged* or *discriminated* union, in the sense that the set represented by all possible instances of `A + B` contains all possible instances of `A` and all possible instances of `B`, even if `A` and `B` are equal; say they're both `Int`: `left(2)` and `right(2)` produce a *different* instance for `Int + Int`. Another divergence in names: I used *coproduct* to refer to *sum types* or *discriminated/tagged unions*, and I did it because I think that the category-theoretical approach is more informative and precise.

In adding methods and utility functions to `Product<A,B>`, I purposely always talked about a "product", but some types have a specific name for a reason: [denotational semantics](https://www.youtube.com/watch?v=pOl4E8x3fmw) can help us infer *precise* meaning from the code. Broadly speaking, denotational semantics refer to the meaning (actual, precise meaning, with no effects on the side, and no surprises) of expressions written in code, like for example a *method call on a instance of a type, with some arguments*. To convey better meaning we can use specific names for specific types, and attach specific semantics to those types.

First of all, `.map` is the fundamental composition method of a [`Functor`](https://en.wikipedia.org/wiki/Functor). There's a lot to talk about (a to love) about functors, but that would be beyond the scope of this essay. Let's just say that `.map` must respect some laws to be meaningful:

```swift
let p: Functor<A> = ...

/// law 1
p.map { $0 } === p

let f: (A) -> B = ...
let g: (B) -> C = ...

/// law 2
p.map(f).map(g) === p.map { g(f($0)) }
```

These laws simply state that composition of functions is preserved when mapping. All the types I've presented in this essay are functors, in some way or another. Specific types have different additional properties, though.

For example, `Product<A,B>` with `.extend` and `.extract` as we used it the course of the article, can be referred to as a `Coreader<A,B>` *comonad* if we attach some *laws* to it that describe the way its instances behave; talking about the axioms - that must hold for a comonad to be called in such way - is beyond the scope of this article, but we can synthetically say that a `Coreader<A,B>` behaves if the following expressions are true:

```swift
let p: Coreader<A,B> = ...

/// law 1
p.extend { $0.extract } === p

let f: (Coreader<A,B>) -> C = ...

/// law 2
p.extend(f).extract == f(p)

let g: (Coreader<A,C>) -> D = ...

/// law 3
p.extend(f).extend(g) === p.extend { g($0.extend(f)) }
```

These laws are basically about making method composition *commute*, in the sense that two different compositions that should result in the same value **shall** result in the same value, and they force us to write methods like `.extend` and `.extract` in a *pure* way. For more about `Coreader` and its dual `Reader` (there was a *co-* there, so of course it was the dual of something) please watch [this](https://www.youtube.com/watch?v=oPyqKETp3ks).

The other possible application we saw for `Product<A,B>` results in what's typically referred to as the `Writer<A,B>` *monad*, with a caveat: we needed to provide a way to "combine contexts" generically, and we ended up using an array, that combines pretty naturally just by concatenation. In general, we need that the `A` in `Writer<A,B>` is generically composable and has some *empty* instance that can be constructed with no arguments: these are the properties of a `Monoid`, and I talked about it [in a previous post](https://broomburgo.github.io/fun-ios/post/on-abstraction/).

`Writer<A,B>`, being a *monad*, has `.flatMap` and `.pure`, and there are some laws for those too:

```swift
let f: B -> Writer<A,C> = ...
let x: B = ...

/// law 1
Writer<A,B>.pure(x).flatMap(f) === f(x)

let p: Writer<A,B> = ...

/// law 2
p.flatMap { Writer<A,B>.pure($0) } === p

let g: C -> Writer<A,D> = ...

/// law 3
p.flatMap(f).flatMap(g) === p.flatMap { f($0).flatMap(g) }
```

These laws exist for the same reason as those for the comonad, and insure that composition commutes without any weird effects related to the particular composition path we choose.

Finally, `Coproduct<A,B>`, if we consider `.left` a failure case and `.right` the *happy path*, forms a monad (the `.pure` function in this case corresponds to `.right`) usually called `Either<A,B>`, or more recently (and I think, more correctly) `Result<A,B>`. It must follow the same identical composition laws, just switch `Writer` with `Result`.

In the example with `Observable` we're not favoring a path over the other, and in fact we didn't use `.flatMap` for the `Coproduct`, but only `.map` and `.fold`: in fact, we distinguished `.mapLeft` and `.mapRight`. This makes `Coproduct<A,B>` a *bifunctor*, in the sense that it can be mapped in two ways. I didn't provide an example, but also `Product<A,B>` is a bifunctor when we're not considering any projection "special" in respect to the other.

I wanted to avoid getting lost in jargon during the course of this essay, but there's value in calling things with their names: it unlocks amazing possibilities to share similar (sometimes, identical) concepts between different contexts, and provide a shared language for abstraction and composition.

Until next time.