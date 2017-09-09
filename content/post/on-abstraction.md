+++
date = "2017-09-09T20:10:00+00:00"
title = "On Abstraction"
type = "post"
draft = false
+++

> This post is a direct answer to [this other post](http://belkadan.com/blog/2017/09/Over-abstraction/), which itself is mostly an answer to a bunch of discussions on Twitter, talks and more posts. While this post has probably value by itself, it's probably better to read the original post first, because I'm going to refer to some of its statements.

---

There's an interesting discussion going in the functional Swift community: whether using abstract data types, like [Monoid](https://github.com/typelift/Abstract/blob/master/Sources/Abstract/Monoid.swift), is actually useful, and how this relates to the way protocols are usually laid out in the standard library. The peril seems to be a downward spiral into "over-abstraction", that is, roughly, the reliance on software techniques that are so far removed from the concrete, underlying problems, that cost way too much in terms of intellectual reasoning without providing proportionally better results.

The following piece is my take on the matter, and I thank [the author](https://twitter.com/UINT_MIN) of the original article to have bootstrapped the discussion.

## A matter of perspective

First I would argue that there's no such thing as "over-abstraction", because there's no measurable threshold: even if we assume that "abstraction" means "protocol" there's no way of defining a specific protocol as "too abstract". It really depends on how one's expectancy for the "contract" that the interface should help holding actually relates to the original intention and semantics behind the interface itself. But if by "over-abstraction" the author meant *wrong* abstraction I 100% agree: not all abstractions are created equal, and most of the discussion on abstract data types is actually directed toward finding what are the *right* abstractions.

The article takes `Sequence` and the fact that `Set` conforms to `Sequence` as an example of problematic abstraction, because `Sequence` declares methods like `dropFirst` but `Set` is unordered so the *first* element is undefined. But that doesn't seem a problem to me: `Sequence`, like `Monoid`, is not a contract; it's just a way of saying that a certain, concrete type, will allow a client to access all of its element in a sequential fashion, and that's it. It doesn't say anything about the order of the elements or their shape (in fact the element type is an associated type): things like these depend on the **concrete** type that implements `Sequence`. If the concrete type is `Set` or `Dictionary`, the first element will be undefined, and this depends on the concrete type itself. The **only** guarantee of `Sequence` is that all the elements will be accessed: `Sequence` doesn't even guarantee that the iteration will be non-destructive. But `Set` or any other type conforming to `Sequence` must guarantee that all the contained element will be accessed by a `for-in` cycle, and this is the only information that I can use if the only thing I know about a generic instance is that it's a `Sequence`. Notice that a function like this:

```swift
func doSomethingWithAllTheElements<S>(of sequence: S) where S: Sequence {
	/// code
}
```

could never terminate because a `Sequence` could be infinite. The potential bug depends on the concrete type that's being passed to the function, and in this case `Sequence` probably is too shallow as a requirement.

Abstractions like `Sequence` are not just good because they let you reuse operations: that's not the point, the point is to specify some semantics for the underlying concrete type, and it's important to identify what those semantics are. *Then* you can define reusable operations, that would be meaningless without the explicit semantics. The `doSomethingWithAllTheElements` should take a `Collection`, that is guaranteed to be finite (because an `endIndex` must be provided). But if the function was `doSomethingWithTheFirst10TheElements` a `Sequence` would be fine: which the first 10 are depends on the concrete type. If we *really* needed some kind of ordering, probably something like this would be better:

```swift
func doSomethingWithTheFirst10ElementsAfterReordeing<C>(of collection: C) where C: Collection, C.Iterator.Element: Comparable {
	/// code
}
```

This way we are enforcing that we can `sort` the `Collection`, but the way it is sorted depends on how the collection elements implement `Comparable`: again, the actual implementation is left to the concrete type, we are just requiring certain semantics (for `Comparable`, a strict total order) and the ability to call certain methods, that without the semantics attached would be meaningless.

> A note: all this makes sense if we ask a programmer to be **precise** with their definitions, and in general abstract data types are concocted exactly for this reason: the will to be as precise as possible, thus improving one's ability to reason about code.

The original article contains an example in which is shown that when talking about animals the appropriate words to use for calling them, taken from their biological hierarchy, depend on the context: sometimes a cat is better referred as "a cat" but in other contexts "mammal" could be better, and in other "Siamese cat" is the way to go. Totally agree, and this is exactly the point: we lay out these hierarchies of abstractions so that we can pick the level that we actually need for a certain process. But in programming this might actually lead to "under-abstraction", and not the opposite. Take for example [this pull request](https://github.com/typelift/Abstract/pull/28): the author argues that `OptionalM<M: Monoid>: Monoid` should really be `OptionalM<S: Semigroup>: Monoid` because there's no requirement for the generic type to be `Monoid`, it's fine for it to be `Semigroup`. And he's right: we can use a more general (thus, of higher order) abstraction for the generic parameter. But we couldn't require for it to just be `Magma`: that would be over-abstracting, and it wouldn't work.

One of the many differences between programming and natural language is that the mathematical roots of the former will allow a programmer to separate what makes sense from what doesn't in a very precise and foolproof way: we can still argue that "mammal" is not a useful term for a cat in many contexts, but in programming we can *prove* it more formally.

## The good, the bad and the ugly abstraction

I pulled an example from an abstract data types library on purpose, because I really want to talk about what's good about those things, more than comparing them with different approaches.

Building "meaningful" generic operations, while can be done and in fact it is, is not the point of defining and enforcing protocols like `Monoid`. The generic operations only follow from the semantic power acquired when we try to be precise with these concepts. By that I mean that `Monoid` comes **before** types already in the standard library (on anywhere else) that have a *natural* way to conform to it. We know for example that `String` and `Array` conform naturally to `Monoid` in the following way:

```
Monoid => String
<> => +
empty => ""

Monoid => Array
<> => +
empty => []
```

But we don't define `Monoid` just because we have a couple of types that could take advantage of it. We define `Monoid` because we want to require a completely generic *composition* operation that is associative and as a *neutral* value.

For example, a transitional value that represents some kind of feedback left from a user could be a `Monoid`, in the sense that partial instances of it could be composed in an associative way:

```swift
struct UserData {
	let lastAccess: Date
	let numberOfAccesses: Int
	let feedbacks: Array<String>
}
```

We would like for this type to be a monoid, so that if we have a stored value and a new one comes, that could be itself an aggregate of multiple accesses, we have a way to generically compose those values so that some properties hold.

We could for example take always the highest date, add the number of accesses, and concatenate the feedbacks. Or we could redefine `UserData` like this:

```swift
struct UserData {
	let lastAccess: Max<Date>
	let numberOfAccesses: Add<Int>
	let feedbacks: Array<String>
}
```

What are `Max` and `Add`? Are *concrete types* that have a particular implementation of `Monoid`, like `Array` (that is already in the standard library). Now, because we can *prove* that a product of monoids (a struct or class in which all the properties are monoids) is itself a monoid if we compose the things in the most natural way, we can simply write:

```swift
extension UserData: Monoid {
	static func <> (left: UserData, right: UserData) -> UserData {
		return UserData.init(
			lastAccess: left.lastAccess <> right.lastAccess,
			numberOfAccesses: left.numberOfAccesses <> right.numberOfAccesses,
			feedbacks: left.feedbacks <> right.feedbacks)
	}
	
	static let empty = UserData.init(
			lastAccess: Max<Date>.empty,
			numberOfAccesses: Add<Int>.empty,
			feedbacks: Array<String>.empty)
}
```

Notice that this code could have been written automatically by a code generation tool like [Sourcery](https://github.com/krzysztofzablocki/Sourcery).

Now, one could argue that this could have been done in dozens of other ways: I 100% agree, and I'm certainly not interested in asserting that this is *the best way* of doing it. What I'm arguing is that a concept like `Monoid` emerges when we're discussing about what are *good abstractions* for representing a generic computational concept like *composition*, and tapping into decades of mathematical research (and modern mathematics is basically the science of abstraction) seems like a good idea. When discussing abstract data types and their applications in software development we're not interested in finding already existing concrete types that somehow already conform to those abstractions, but in defining new concrete types that could conform to the abstractions in interesting ways. That doesn't mean that some *classic* existing types don't naturally conform: for example a `(A) -> A` is obviously a monoid, but the interesting thing is that also a `(A) -> B where B: Monoid` is a monoid (in Swift these must be encapsulated in a struct).

This misunderstanding could have led the author of the original article to observe that there's no point in having a `Additive` protocol that just support addition: in the context of abstract algebra `Additive` has no meaning by itself, but it's only used together with `Multiplicative` to define `+` and `*` operations (that are **not necessarily** sum and product) in which the `*` is *distributive* over `+`, thus:

`a*(b + c) = a*b + a*c` 

where `a`, `b` and `c` may or may not be numbers. What I'm personally finding more and more in my work is that these abstractions seem to have the magical property to pop up time after time in places were I initially had defined (like every programmer does) custom interfaces. As iterating a bunch of objects is a powerful generic idea, also the basic concept of "composition" with a distinguished "empty" element seems to be extremely pervasive and useful. Thus, I'd rather tap into existing literature and conventions instead of coming up with new words to express the same things, which is notably [the hardest thing in computer science](https://martinfowler.com/bliki/TwoHardThings.html).

## Conventional notation is one less problem to solve

Why `<>`? Easy, it's **convention**. There's no particular reason to use a specific operator, but actually **there are reasons** to use an operator instead of a method or a named function.

An operator is useful in a specific case: when the operation that it represents is repetitive. This:

```swift
1 + 2 + 3 + 4
``` 

is a lot better than this:

```swift
1.adding(2).adding(3).adding(4)
```

Using an operator for a specific operation that must be repeated several times improves the code readability by a large margin, there's simply less noise and more focus on the actual information content. The only sensible way to call a method that meant what `<>` expressed would probably be `compose` or `combine`, or even `putTogether` because that's what the operation represents by itself, and nothing else.

But of course what's happening here:

```swift
Add(1) <> 2 <> 3 <> 4
```

is something completely different than what's happening here:

```swift
Max(1) <> 2 <> 3 <> 4
```

Shouldn't we use different operators, or (at that point) methods? No, because what `<>` means depends on the *context*: the whole meaning of the operation is entrusted to the **concrete type**. This is something that might confuse (at first) because in OOP we're used to verbose names for methods and classes, that are basically required because we try (and usually fail) to express the entire semantic content of a arbitrarily complex domain-specific operation just by using a few english words.

So for example, if I see this code (taken from [one the articles from which the original article draws inspiration](http://www.fewbutripe.com/swift/html/dsl/2017/06/29/composable-html-views-in-swift.html)):

```swift
let fullArticle: View<Article, [Node]> =
  articleHeader
    <> articleBody
    <> articleFooter
```

to understand it I need to know exactly what `View<A,B>` is, but once I know it, I can fully predict what `fullArticle` will be. The meaning of `<>` depends on the context.

The suggestion behind using abstract data types as protocols to be implemented by concrete types that represent specific operations is that, if again our goal is to be as precise as possible, we should define simple, precise operational Lego pieces and build our domain-specific objects by composing them. Is this precision really needed? I don't know, maybe, but it can be argued that it might unlock a much greater ability for us to actually reason about the code that we write.

The study behind abstract data types and their usage in practical programming contexts (like the development of an iOS app) is about the protocols themselves, and not about finding abstractions to encapsulate existing concrete behaviors. It's also about defining concrete types that interpret the *composition* operation in a specific way, and about finding useful ways to combine these types. In fact, usually the concrete applications follow naturally where there already were specialized types with custom interfaces that required writing a bunch of code to express the same algorithm over and over again.

There are certainly other approaches to the resolution of generic problems, but the one based on abstract data types looks really promising, and it's the one I found to work best in my experience for a range of applications.