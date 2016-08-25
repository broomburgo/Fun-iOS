+++
date = "2016-08-25T10:38:47+02:00"
draft = true
title = "Code reuse: a primer"
categories = ["essay"]

+++

[Last time](http://engineering.facile.it/optionals-in-objective-c/) we looked at a possible implementation for the [Optional](https://developer.apple.com/library/prerelease/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-ID330) type in Objective-C; while the main point was to port to Objective-C a tool that's frequently used in Swift, making use of the `Optional` class can be considered an application of a much more general concept: **code reuse**. In fact, `Optional` is not tied to a particular domain, and can be reused over and over again in multiple projects: that's what actually happens in Swift. But, to think about it, that's what happens for a wide range of *classes* in Objective-C, or *types* in Swift: for example, `NSArray` and `Array` are both constructs that expose a certain interface, have a certain implementation, and are reused multiple times within methods and functions. And again, `NSArray` and `Array` are not tied to a particular domain, and have two important properties:

- they are **generic**, that is, they are not dependent on a particular subtype: an array of numbers and an array of strings will work in the exact same way when it comes to behavior that's strictly associated to arrays, like counting elements, removing or adding an element, mapping, filtering, reducing et cetera;
- they are **composable**, that is, they can be stacked on top of each other, and assembled with other more specific things, while keeping the same **predictable behavior**: an array of *customers*, each with their own array of *purchased products*, can be mapped to an array of arrays of *products*, that can be flattened into an array of *products*, that can be reduced into a number representing the total cost;

`Array`, like `Optional`, is a nice example of perfectly reusable code, but there are many more constructs that satisfy the aforementioned rules: *genericity* and *composability*. What doesn't satisfy the rules is domain-specific objects, even when it seems like an object is sufficiently generic to be reused. A `Customer` class, for example, could seem a good candidate, but trying to reuse the same class in two different projects will result in **bad news** and headaches: either we'll need to **specialize** the class for each project, making the two diverge, or we'll need to add levels of **indirection**, because we're forcing an object into a project it doesn't belong to.

There's another thing to consider: if we really want to reuse code, we'll also need to make **design choices** that allow that code to be reused. I could actually forget about the `Array` and `Optional` types, and create classes that never expose their underlying components; for example, I could create a class that represents a collection of optional objects, without ever exposing in the interface the fact that I'm using `Array` and `Optional` for its implementation: it would result in some serious **contortion** of methods' signature, but it's possible. Also, is it a good idea? **I don't think so**. Creating a specific, incomposable class for every possible need will result in thousands of lines of what's essentially **boilerplate** code, complicated interfaces, and ridiculously long class names. If we aspire to code reuse, other than the two rules we just defined, we also need to pay attention to the way we architect our apps, and the main design principle to follow here is one of **composition**, that can be summed up with the following:

> Domain-specific constructs and behaviors should be realized by composing atomic and generic building blocks.

This basically means that, instead of creating every time a specific implementation for our needs, we should try and satisfy that need by composing reusable, atomic objects. This looks complex, and at this point we should ask ourselves if code reuse is really something we want to pursue. My answer is a definitive **yes**, for the following reasons:

- with reusable components we can **encapsulate** a particular behavior, and avoid rewriting essentially the same thing over and over again;
- we'll have a **guideline** for designing the architecture of our app, because we wont't need to constantly define new interfaces for the same behaviors;
- a reusable component is very easy to **test**, because it's small and atomic, so by using it we will be more confident about the correctness of our code;

That, again, is what happens when we use types like `Array` and `Optional`. As I said, there are many more of these building blocks, and actually the definition of their interfaces is a problem in itself, and has no obvious solution, but I think it's a very interesting problem to tackle. By following the *generic* and *composable* rules we already have a guideline for designing them, but if we want to create our owns we should probably add one more rule:

- a reusable object has to be **simple**, that is, it should have a unique, linear, easily describable responsibility;

`Optional` follows this rule: it's a generic container that represents an object that could be there, or not. `Array` follows this rule: it represents and *ordered* collection of objects, each one them accessible in constant time. Let's try to apply the rule and create a new reusable object.

Consider this real, kind of frequent use case: different objects must be notified when a certain object changes state or does something. A way to solve the problem is represented by the [observer pattern](https://en.wikipedia.org/wiki/Observer_pattern), that is, make our objects implement an interface that exposes a [publish-subscribe](https://en.wikipedia.org/wiki/Publish–subscribe_pattern) behavior. The point is, implementing an interface **is not reusable**: each time we want to use this pattern we are going to **rewrite** very similar code; instead, we want to encapsulate the behavior in a generic object, and reuse that object instead. What follows is a possible implementation, the one that I use in production, but there are many more: actually, [functional reactive programming](https://en.wikipedia.org/wiki/Functional_reactive_programming) has spawned from this very problem, that is, finding a generic, composable way of representing and manipulating observable streams, but in this case we're going to create a much simpler **Signal** object.

The `Signal` class we want to implement is going to represent an *update* for a certain object: that update can be everything, like a new value for a property, or an action that was taken. We'll use the Swift language, which is also going to give us a lot of **type-safety** for free. Here's a summary of what we want for the `Signal` class:

- it has to a have a **parametric** subtype, that is, the type of the value that will be signaled in each update;
- it has to have an `observe` method, that takes a closure representing an action that has to be taken every time an update is given;
- it has to have a `send` method, which takes a new value of the underlying subtype;
- it has to have some simple methods for composition, that follow the usual naming conventions, like `map`, for generating a new `Signal` from an existing one by transforming the observed value, and `forwardTo` to make another `Signal` *trigger* when a certain `Signal` is producing a new value;

Semantically, we can describe a `Signal` as a container for a value that's going to be there at a certain point in time, and it's going to update itself indefinitely.

What follows is the entire implementation of the `Signal` class:

``` swift
public enum SignalContinuation
{
  case Continue
  case Stop
}

public class Signal<Subtype>
{
  private var observers: [(Subtype -> SignalContinuation)] = []

  public init() {}

  public func observe (observeFunction: Subtype -> SignalContinuation)
  {
    observers.append(observeFunction)
  }

  public func send (value: Subtype)
  {
    var continuations: [(Subtype -> SignalContinuation)] = []
    while observers.count > 0
    {
      let observer = observers.removeFirst()
      let continuation = observer(value)
      switch continuation
      {
      case .Continue:
        continuations.append(observer)
      case .Stop: break
      }
    }
    observers = continuations
  }

  public func forwardTo (otherSignal: Signal<Subtype>) -> Signal
  {
    observe { action in
      otherSignal.send(action)
      return .Continue
    }
    return self
  }

  public func forwardTo <OtherSubtype> (
    otherSignal: Signal<OtherSubtype>,
    mappingFunction: Subtype -> OtherSubtype) -> Signal
  {
    observe { action in
      otherSignal.send(mappingFunction(action))
      return .Continue
    }
    return self
  }

  public func map <OtherSubtype> (transform: Subtype -> OtherSubtype) -> Signal<OtherSubtype>
  {
    let mappedSignal = Signal<OtherSubtype>()
    forwardTo(mappedSignal, mappingFunction: transform)
    return mappedSignal
  }
}
```

This is really simple, but still, really powerful. A problem that's frequently found when implementing the observer pattern is how to manage unsubscriptions; it's a responsibility of the observer object to stop observing something, and in this `Signal` implementation this is easily managed within the closure passed to the `observe` method: the closure must return a `SignalContinuation` value, that can be simply `.Continue` (that is, keep observing updates) or `.Stop`. Another problem is memory management: we need to make sure that when an observer's memory is released, it will also stop observing, or a message will be sent to a dangling pointer, resulting in the app crashing. Swift's `weak` memory semantics actually makes this really easy to do: we'll put a `guard` clause at the beginning of the closure passed to the `observe` method; if the object has become `nil`, we'll simply return `.Stop`. The following example shows a simple use of the `Signal` class, including the *stop-observing-on-nil* mechanism:

``` swift
class Sender
{
  let signal = Signal<Int>()

  func sendNew(value: Int)
  {
    signal.send(value)
  }
}

class Receiver<Type: CustomStringConvertible>
{
  func startObserving(signal: Signal<Type>)
  {
    signal.observe { [weak self] value in
      guard let this = self else { return .Stop }
      this.printNewValue(value)
      return .Continue
    }
  }

  func printNewValue(value: Type)
  {
    print(value)
  }
}

let sender = Sender()
let receiver = Receiver<Int>()

receiver.startObserving(sender.signal)

sender.signal.send(3)
sender.signal.send(5)
sender.signal.send(10)
sender.signal.send(20)
/// this will print 3, 5, 10, 20 on console
```

In the example we can see an application of the concepts we talked about at the beginning of this article: instead of creating yet another interface for the same behavior, we are directly using and reusing the `Signal` object. A more complex example would be the addition of something like a *resonator*, that is, an object with a `Signal` that resonates with another, like in the following example:

``` swift
class DoublingResonator
{
  let signal = Signal<Int>()

  func resonateWith(otherSignal: Signal<Int>)
  {
    otherSignal.forwardTo(signal) { $0*2 }
  }
}

let resonator = DoublingResonator()
resonator.resonateWith(sender.signal)

let receiver = Receiver<Int>()

receiver.startObserving(resonator.signal)

sender.signal.send(3)
sender.signal.send(5)
sender.signal.send(10)
sender.signal.send(20)
/// this will print 6, 10, 20, 40 on console
```

There are many other options for composition of signals, but as long as we don't need them, it's better to keep the class **simple**: then, gradually over time, we can start adding features to the class, and as long as they're properly generic and tested, we will be able to use them in all of our projects. 

To conclude, finding the right abstractions for reusability is of course a problem, and the solution is not an easy one: plenty of academic papers address the problem in several ways (classic Charles W. Krueger's paper [Software Reuse](http://www.biglever.com/papers/Krueger_AcmReuseSurvey.pdf) contains a good overview of the used techniques), and the reason why [category theory](https://en.wikipedia.org/wiki/Category_theory) has many applications in functional programming is because it offers an excellent set of abstractions for tackling several classes of problems. But still, I think the advantages of code reuse are many, and that achieving a compositional design through atomic, reusable components is **a worthy goal** to pursue.
