+++
date = "2016-12-29T11:19:50+01:00"
title = "Lenses and Prisms in Swift: a pragmatic approach (old version)"
type = "post"
draft = true
+++

> This article is a little outdated and doesn't really represent anymore the actual way I use these concepts in my day-to-day work. I'm going to update it soon with some of the concepts expressed in my talk [Lenses and Prisms in swift](https://github.com/broomburgo/Lenses-and-Prisms-in-Swift).

---

The concept of [functional lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing) has become pretty popular in **functional programming** circles, and there are already good contributions for applying lenses to other, traditionally imperative/OO contexts, like [Javascript](https://medium.com/@dtipson/functional-lenses-d1aba9e52254#.wv5xkpy7a). Kickstarter's own [Brandon Williams](http://www.fewbutripe.com) has done an excellent job in introducing lenses to the [Swift community](https://www.youtube.com/watch?v=ofjehH9f-CU), and in showing [practical examples](https://www.youtube.com/watch?v=A0VaIKK2ijM) when working with something like `UIView`, a radically **OOP construct** that an iOS developer has to work with on a daily basis.

I'd like to offer a more in depth view on why lenses can be useful in Swift, and also talk about a similar/different thing called **Prism**: it's likely that everyone that's interested in lenses has heard about prisms but it seems like, while the basic intuition behind a lens can be grasped, the one behind a prism is kind of obscure. But before that, it might be important to discover again the concept on **Lens**: I'd like to do that because, by reading the comments for [this](https://www.youtube.com/watch?v=ofjehH9f-CU) video, I understand that **there's some confusion**, mixed with the usual hostility towards functional programming concepts by some OOP people (luckily, a minority) who consider functional programming pointlessly complex: it's really the opposite, that is, it's simple but initially hard to grasp. And simple/complex [is not the same thing](https://www.infoq.com/presentations/Simple-Made-Easy) as easy/hard.

So, let's start from the beginning and talk about lenses, but in a more pragmatic way, that is, let's consider some **practical problems** and see if a traditional, idiomatic (and imperative) Swift approach can be enough or not. The code from this post is available on [GitHub Gist](https://gist.github.com/broomburgo/bdc956243be2c3806a3d38e4d207008b), and I strongly suggest to follow the Gist while reading the post, even if I'm still going to recall most of the code here.

## Retrieval and mutation of values in idiomatic Swift

First of all, a quick word on **immutability**: using immutable objects is a [very](http://wiki.c2.com/?ValueObjectsShouldBeImmutable), [very](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html) good idea when those objects represent some kind of data structure, and to do this when working with classes in Swift we must have only `let` (constant) properties. But Swift `struct` and `enum` have *value semantics*, that is, they're not passed around as reference but as value, thus they're deep copied when passed from a context to another. This means that we really don't need to have immutable structs: the only thing that's needed is *immutable variables* (thus, **constants**) that hold those structs. Of course we can make the variables mutable if we need to modify the data structure somehow: not a problem, because when modifying an object with value semantics, actually a new, deep copied object will be created, and the **copy-on-write** mechanism will assure us that performance and memory usage are both optimal. So, the following example structs will all have `var` properties.

```swift
struct Company {
	var name: String
	var board: BoardOfDirectors
}

struct BoardOfDirectors {
	var ceo: Employee
	var cto: Employee
	var cfo: Employee
	var coo: Employee
}

struct Employee {
	let name: String
	let salary: Salary
}

struct Salary {
	var amount: Double
	var bonus: Double
}
``` 

In our use case, we probably extracted an instance of `Company` from a repository, and because we want to change the CEO's bonus (let's halve it, shall we?), we could manually do this:

```swift
/// example 1

var com = Repository.getCompany
com.board.ceo.salary.bonus *= 0.5
Repository.setCompany(com)
```

This seems perfectly fine, and it's made possible by the fact that every property is a `var`; it's also clear and readable: we can see at-a-glance that we are halving the CEO's bonus. Now we decide that we want to give the CTO a raise, let's say that they're going to earn an average between the CEO and the CFO. First, let's define an `average` function:

```swift
extension Double {
	static func average(_ values: Double...) -> Double {
		return values.reduce(0, +)/Double(values.count)
	}
}
```

Then, let's execute the required action:

```swift
/// example 2

var com = Repository.getCompany
let ceoSalary = com.board.ceo.salary
let cfoSalary = com.board.cfo.salary
com.board.cto.salary.amount = Double.average(ceoSalary.amount,cfoSalary.amount)
Repository.setCompany(com)
```

It seems *kind of* ok: it's not really clear what we're doing by just glancing at the code, because the logic is not clearly expressed, and it's not separated from the internal workings of the code. The fact that we need **a series of statements** in a certain order, and that we need to hold values in temporary constants to make the thing more readable are just **noise** that obfuscate the logic. Let's see a final example. We want to change the bonus of the CFO this way: we want to make sure than the bonus is not less than the 6% of their salary; if it is, set the bonus to 6% of the salary; if it's not, leave it alone.

```swift
/// example 3

var com = Repository.getCompany
let cfoAmount = com.board.cfo.salary.amount
var cfoBonus = com.board.cfo.salary.bonus
if cfoBonus/cfoAmount < 0.06 {
	cfoBonus = cfoAmount*0.06
}
com.board.cfo.salary.bonus = cfoBonus
Repository.setCompany(com)
```

Now the logic is mostly lost in the code workings for retrieval and setting, and we also need to create a temporary variable `cfoBonus` that will increase the probability of making mistakes. We would really like to separate the logic needed to *get* and *set* the needed properties, from the *business logic*, that is, the *rules* that we want to apply to our data. *Lenses* have exactly this purposes: they atomically express the *get* and *set* operations.

## Using lenses to abstract `set` and `get` operations

The reason why they're called "lenses" is that the basic intuition is the one of **focusing** on a particular part of a data structure: once we focused on that, we can retrieve it and change it with ease. Also, lenses are *composable*, which means that if a lens acts on a structure `A` by focusing on the subpart `B`, and another lens acts on a structure `B` by focusing on the subpart `C`, we can *join* them to create a lens that acts on a structure `A` by focusing on the sub-subpart `C`, which is of course great for nested data structures. And here lies the power of lenses: composition, to create more complex actions from simpler ones.

I'm reporting the full implementation of lenses that I'm going to use here, please refer to the companion Gist for clearer code:

```swift
protocol LensType {
	associatedtype WholeType
	associatedtype PartType

	var get: (WholeType) -> PartType { get }
	var set: (PartType,WholeType) -> WholeType { get }

	init(get: @escaping (WholeType) -> PartType, set: @escaping (PartType,WholeType) -> WholeType)
}

struct Lens<Whole,Part>: LensType {
	typealias WholeType = Whole
	typealias PartType = Part

	let get: (Whole) -> Part /// get the "focused" part
	let set: (Part,Whole) -> Whole /// set a new value for the "focused" part

	init(get: @escaping (Whole) -> Part, set: @escaping (Part,Whole) -> Whole) {
		self.get = get
		self.set = set
	}
}

extension LensType {
	func over(_ transform: @escaping (PartType) -> PartType) -> (WholeType) -> WholeType {
		return { whole in self.set(transform(self.get(whole)),whole) }
	}

	func join<Subpart, OtherLens: LensType>(_ other: OtherLens) -> Lens<WholeType,Subpart> where OtherLens.WholeType == PartType, OtherLens.PartType == Subpart {
		return Lens<WholeType,Subpart>(
			get: { other.get(self.get($0)) },
			set: { (subpart: Subpart, whole: WholeType) -> WholeType in
					self.set(other.set(subpart,self.get(whole)),whole)
		})
	}
	
	func zip<OtherPart, OtherLens: LensType>(_ other: OtherLens) -> Lens<WholeType,(PartType,OtherPart)> where OtherLens.WholeType == WholeType, OtherLens.PartType == OtherPart {
		return Lens<WholeType,(PartType,OtherPart)>(
			get: { (self.get($0),other.get($0)) },
			set: { other.set($0.1,self.set($0.0,$1)) })
	}
}
```

A `Lens` is really just a pair of functions:

- a function to `get` a property from a data structure;
- a function to `set` a new value for a property in a data structure, returning the modified structure.

The power of lenses is mostly in:

- the `join` function, that composes two lenses with matching part/whole;
-  the `over` function, that modifies a property of a data structure based on the previous value, returning the new structure;
-   the `zip` function, that mixes together two lenses with the same whole part, so we can apply both at the same time, by retrieving/modifying two things together. 

Names of functions are mostly arbitrary, and actually it's frequent to use custom operators for them, but I prefer methods. To rewrite the previous examples with lenses, we need to add some lenses to every `struct`: please refer to the companion Gist for the implementation. 

For the first example, things are going to appear actually messier. Let's define a couple of lenses:

```swift
let onCEO = Company.lens.board.join(BoardOfDirectors.lens.ceo)
let onSalaryAmount = Employee.lens.salary.join(Salary.lens.amount)
```

We are going to reuse these a lot, and thanks to the very *lens abstraction* we can generically define objects that will somehow act on a certain part of a data structure. The *action* required in `example 1` is to halve the CEO's salary amount, something that we can easily express by combining lenses, without the need of a living variable instance of `Company`:

```swift
let action: (Company) -> Company = onCEO.join(onSalaryAmount).over { $0*0.5 }
``` 

Notice that I specified the **type** of `action`: it's a function that transforms a `Company`. In fact, pretty much everything we did in the previous examples was to take the current company, put it into a `var`, that is, a mutable reference, modify something, and then store the company again. We can abstract the whole process with the following:

```swift
func updateCompany(with action: (Company) -> Company) {
	Repository.setCompany(action(Repository.getCompany))
}
```

The `update` function takes a `(Company) -> Company` function as an input, and it's very easy to generate those with lenses, no mutable references needed. The first example then becomes:

```swift
/// example 1 with lenses

updateCompany(with: onCEO.join(onSalaryAmount).over { $0*0.5 })
```

For the second example, let's define another couple of completely reusable lenses:

```swift
let onCTO = Company.lens.board.join(BoardOfDirectors.lens.cto)
let onCFO = Company.lens.board.join(BoardOfDirectors.lens.cfo)
```

With these, the second example becomes:

```swift
/// example 2 with lenses

updateCompany {
	onCTO.join(onSalaryAmount)
		.set(Double.average(
			onCEO.join(onSalaryAmount).get($0),
			onCFO.join(onSalaryAmount).get($0)),
				 $0)
}
```

This looks messier on first sight, but the problem is really one of familiarity, because here, without creating temporary variables, we're pretty clearly defining the logic:

- `updateCompany`: we're going to update a `Company`;
- `onCTO.join(onSalaryAmount)`: by acting on CTO's salary amount;
- `.set(Double.average`: it's going to be an average;
- `onCEO.join(onSalaryAmount).get` and `onCFO.join(onSalaryAmount).get`: of the salary amount of the CEO and CFO.

I'd say that the logic is clearly expressed if you're familiar with the constructs, and in general with working with **higher-order functions**, but the case still doesn't seem to be particularly on point.

Let's see the third example:

```swift
/// example 3 with lenses

updateCompany(with: onCFO.join(onSalaryAmount)
	.zip(onCFO.join(onSalaryBonus))
	.over { (amount,bonus) in
		if bonus/amount < 0.06 {
				return (amount,amount*0.06)
			} else {
				return (amount,bonus)
			}
})
```

Something's happened: we are mixing together actions from `onCFO.join(onSalaryAmount)` and `onCFO.join(onSalaryBonus)`, because the logic needs both, and in the `over` method we are pretty clearly expressing the logic, away from boilerplate and code machinery. I think we can pretty clearly see here that by putting the logic needed to *access* the properties of a data structure at any level of depth away from the main business logic, we can write more readable, maintainable and safe code: actually, safety is given mostly because the examples with lenses feature *a single expression* that's type-checked, and not multiple temporary variable and/or multiple statements that must resolve in a certain order.

But still, from a pragmatic standpoint, this seems decent but not great. I'm probably not making a strong case for lenses in Swift, and that's mostly thanks to the value semantics of structs. But notice that if our structs had immutable properties, the code in the regular examples would change pretty radically, and in a bad way, while the code with lenses would be **exactly the same**: lenses don't care about immutability, because the set/get part is abstracted away, and the only thing that we need to care about when working with them is the actual business logic. Just try it yourself by copying the Gist in a playground: switch some properties to `let`, and watch the imperative world burn. Of course we'd face the same problems with classes instead of structs: in working with classes, it would be better to have `let` properties to make the classes immutable, and in that case lenses would really save the day.

Finally, I think there's great value in being able to **express a simple action with a function**, and more complex actions by composing those simple functions: that's one of the main reasons why I develop software in a functional style. But I can understand at least a bit of diffidence, if not complete refusal, towards a concept like "lens" that at first seems so abstract and pointlessly hard to understand. Still, I hope I was able to make a case for using lenses for your data structures.

## Traversing a loosely typed data structure

A problem arises when we're working with a data structure that's not completely defined in terms of **types**. For example, when we **deserialize** some JSON data with `JSONSerialization`, we get an object of type `Any`: usually a server will output a dictionary, that is, a collection of key-value pairs, where keys are strings, and values are whatever the documentation tells. But everyone that's ever worked with JSON knows that in parsing a JSON is not a good idea to just **assume things** and force-unwrap everything: in fact, popular Swift libraries like [Argo](https://github.com/thoughtbot/Argo) make possible to parse JSON in a type-safe and error-aware way.

Now, what happens when we apply lenses to a `[String:Any]` dictionary? For example, we've been told that there's a key `user`, at which there's another `[String:Any]` containing the various properties of a user, like the name, a `String` at the key `name`. A lens could seem a good strategy to retrieve the name, or to generate a new structure with a different name et cetera, but what are the "whole" and the "part" here? Well:

- in the first case, both the whole and the part are `[String:Any]`, because we have a dictionary and we expect another dictionary at the key `user`;
- in the second case the whole is again `[String:Any]`, but the part is a `String`.

Unfortunately, if we try to create a generic lens for this use case we end up with the following:

```swift
typealias AnyDict = [String:Any]

func anyDictLens<Part>(at key: String, as: Part) -> Lens<AnyDict,Part> {
	return Lens<AnyDict,Part>(
		get: { (whole: AnyDict) -> Part in whole[key] as! Part },
		set: { (part: Part, whole: AnyDict) -> AnyDict in
			var m_dict = whole
			m_dict[key] = part
			return m_dict
	})
}
```

In the `get` function we need to force-unwrap the result of `whole[key]`, which is an `Optional<Any>`. We can see that, actually, a lens acting on a `AnyDict` has to be a lens that focuses on an `Optional`. In general, there's no need for a lens to reflect exactly the same types of the structure it's applied upon: in this particular case the part on which the lens focuses is really and `Optional<Part>`. Thus, the correct code is the following:

```swift
func anyDictLens<Part>(at key: String, as: Part) -> Lens<AnyDict,Part?> {
	return Lens<AnyDict,Part?>(
		get: { (whole: AnyDict) -> Part? in whole[key] as? Part },
		set: { (part: Part?, whole: AnyDict) -> AnyDict in
			var m_dict = whole
			m_dict[key] = part
			return m_dict
	})
}
```

Now we can define a dictionary and a couple of lenses:

```swift
let dict: AnyDict = ["user" : ["name" : "Mr. Creosote"]]

let lens1 = anyDictLens(at: "user", as: AnyDict.self)
let lens2 = anyDictLens(at: "name", as: String.self)
```

Notice how we use the keys to define specific lenses, instead of the properties of a struct. This seems fine, until we try to join those lenses to focus at the desired depth of the dictionary:

```swift
let nameLens = lens1.join(lens2) /// this won't compile
```

To join two lenses, the `Part` of the first has to be the same as the `Whole` of the second, and **there's no match here**: the part of the first is `Optional<AnyDict>`, while the whole of the second is just `AnyDict`. We could "cheat" by defining a new `tryJoin` function:

```swift
func tryJoin<A,B,C>(_ left: Lens<A,B?>, _ right: Lens<B,C?>) -> Lens<A,C?>
```

But this is ugly: we cannot define this as a method, and we cannot simply compose `get` and `set` like we did with `join`, `over` and `zip` (because we need to unwrap the optionals). This doesn't seem like a proper pattern for a lens. The point here is the following: when we apply lenses on a loosely typed data structure, the focused parts must be considered *either* some object of some type, *or nothing*, in the case where we don't find what we're searching for. That's really the specialty of a *Prism*.

## Prisms in Swift

Let's start with **the basic intuition** behind a prism. The idea behind a lens is the one of *focusing*: I *forget* the whole data structure and only act on a part of it. The prism, instead, is based on the concept of *dispersion* (if you're interested please refer to the [Wikipedia](https://en.wikipedia.org/wiki/Prism) page): I *separate* a data structure in *possible* subparts, and either retrieve a subpart if it's there, or recreate the structure with just that part. Prisms are useful when we're dealing with data structures that are made of one possible part chosen among any, that is, a *sum type*: for example, `Optional` is a sum type, because an optional instance either contains something or nothing, it's one of the two.

The main methods of a prism are:

- `tryGet`, that gets the possible subpart if it's there, or `nil` (thus, it returns an `Optional`);
- `inject`, that generates the full structure with a specific subpart.

We can see the relation with Lens' `get` and `set`. Here's a possible implementation, complete again with `tryOver`, `join` as `zip`:

```swift
protocol PrismType {
	associatedtype WholeType
	associatedtype PartType

	var tryGet: (WholeType) -> PartType? { get }
	var inject: (PartType) -> WholeType { get }

	init(tryGet: @escaping (WholeType) -> PartType?, inject: @escaping (PartType) -> WholeType)
}

struct Prism<Whole,Part>: PrismType {
	typealias WholeType = Whole
	typealias PartType = Part

	let tryGet: (Whole) -> Part? /// get the part, if possible
	let inject: (Part) -> Whole /// changes the value to reflect the part that's injected in

	init(tryGet: @escaping (Whole) -> Part?, inject: @escaping (Part) -> Whole) {
		self.tryGet = tryGet
		self.inject = inject
	}
}

extension PrismType {
	func tryOver(_ transform: @escaping (PartType) -> PartType) -> (WholeType) -> WholeType? {
		return { whole in self.tryGet(whole).map { self.inject(transform($0)) } }
	}

	func join<OtherPart, OtherPrism>(_ other: OtherPrism) -> Prism<WholeType,OtherPart> where OtherPrism: PrismType, OtherPrism.WholeType == PartType, OtherPrism.PartType == OtherPart  {
		return Prism<WholeType,OtherPart>(
			tryGet: { self.tryGet($0).flatMap(other.tryGet) },
			inject: { self.inject(other.inject($0)) })
	}

	func zip<OtherWhole, OtherPrism>(_ other: OtherPrism) -> Prism<(WholeType,OtherWhole),PartType> where OtherPrism: PrismType, OtherPrism.PartType == PartType, OtherPrism.WholeType == OtherWhole {
		return Prism<(WholeType,OtherWhole),PartType>(
			tryGet: { (whole,otherWhole) in
				self.tryGet(whole) ?? other.tryGet(otherWhole)
		},
			inject: { part in
				(self.inject(part),other.inject(part))
		})
	}
}
```

At this point we can redefine our `anyDict` function in terms of prisms, and generate prisms to use in our dictionary:

```swift
func anyDictPrism<Part>(at key: String, as: Part.Type) -> Prism<AnyDict,Part> {
	return Prism<AnyDict,Part>(
		tryGet: { $0[key] as? Part },
		inject: { [key:$0] })
}

let prism1 = anyDictPrism(at: "user", as: AnyDict.self)
let prism2 = anyDictPrism(at: "name", as: String.self)

let namePrism = prism1.join(prism2)
```

Now `namePrism` can operate on a dictionary like the one defined before:

```swift
let name = namePrism.tryGet(dict) /// it's Mr. Creosote!
```

But a prism can also handle failing cases, like the following:

```swift
let prism3 = anyDictPrism(at: "weight", as: String.self)

let weightPrism = prism1.join(prism3)
let weight = weightPrism.tryGet(dict) /// this is nil
```

That's a lot of code to digest, let's elaborate a little.

The problem with traversing a loosely typed data structure is that, for every `get` call we need to check the retrieved object's type, if there's an object at all: that's going to be **a lot of nested "ifs"**. But we frequently deal with dictionaries, arrays, hashmaps et cetera, with loose type annotations, even in a strongly typed language like Swift: that's because we deal with serialization/deserialization of objects (for example, when we parse the output of a server after a HTTP request), and there's no way to precisely annotate the type of a deserialized object. In fact, `JSONSerialization` works with objects of type `Any` and, idiomatically or not, when working with JSON we need to **safely unwrap** objects if we don't want to incur in possible **crashes**. A prism incapsulates this very concept: traversing a data structure in which at every step we can find a number of possibilities.

Other than `tryGet` there's the inverse operation, `inject`, which builds the whole data structure with the *path* defined by the prism. In our case, we can build a dictionary where there actually is a value for the `weight` key:

```swift
let otherDict = weightPrism.inject("200 kg") /// this is ["user": ["weight": "200 kg"]]
```

Notice that this doesn't depend on `dict`: it's an entirely new dictionary, but the structure of the data is the same (because `weightPrism` still derives from `prism1`). This seems less convenient than the `set` function of a lens, because `set` actually modified an existing structure, but the semantics of a prism is different: two different prisms acting on the same structure could generate different results, so to end up with a single `Whole` we need a way to *merge* them.

To not leave this thread hanging, let's define an `override` function that acts on 2 `AnyDict` and merges them, giving priority to the second in case of matching keys (thus, *override*):

```swift
public func dictOverride(_ first: AnyDict, _ second: AnyDict) -> AnyDict {
	var m_dict = first
	for (key,value) in second {
		if let current = m_dict[key] {
			if let firstAnyDict = current as? AnyDict, let secondAnyDict = value as? AnyDict {
				m_dict[key] = dictOverride(firstAnyDict, secondAnyDict)
			} else {
				m_dict[key] = value
			}
		} else {
			m_dict[key] = value
		}
	}
	return m_dict
}
```

This could be useful when working with types like `[String:Any]`, for example in this case:

```swift
let fullDict = dictOverride(dict, otherDict) /// this is ["user": ["name": "Mr. Creosote", "weight": "200 kg"]]
```

But this has nothing to do with the semantics of a prism: a prism will simply try and follow a particular line in a data structure, or in other words, *traverse* the structure with a certain logic.

## Prisms composition and the `zip` function

This topic deserves it's own section because it's probably the most important one. As with lenses, the true power of prisms comes from the fact that they are composable, i.e., multiple, simple prisms can be combined to obtain more complex ones. For example, the `join` function can be used to create prisms that go *deeper* into a data structure. In the case of dictionaries we could imagine prisms that **explore** the dictionary until they find the requested object. Let's assume, for example, that we're asking a server to provide image urls for products, and that each product is (partially) represented like this:

```JSON
{
  "info" : {
    "image" : {
      "icon_urls" : [
        "https://image.org/1.png",
        "https://image.org/2.png",
        "https://image.org/3.png"
      ]
    }
  }
}
```

In Swift, considering different possibilities:

```swift
let jsonExample1 = [
	"info" : [
		"image" : [
			"icon_urls" : [
				"https://image.org/1.png",
				"https://image.org/2.png",
				"https://image.org/3.png"]]]]
				
let jsonExample2 = [
	"info" : [
		"image" : [
			"icon_urls" : [String]()]]]

let jsonExample3 = [
	"info" : [
		"image" : [
			"WRONG_KEY" : [
				"https://image.org/1.png",
				"https://image.org/2.png",
				"https://image.org/3.png"]]]]
```

Notice the nested structure (we're omitting everything else and just focusing on the image urls). Suppose we just need the first url (if any) from the urls array; then we can define a prism like the following:

```swift
let infoDictPrism = anyDictPrism(at: "info", as: AnyDict.self)
let imageDictPrism = anyDictPrism(at: "image", as: AnyDict.self)
let iconURLsPrism = anyDictPrism(at: "icon_urls", as: [String].self)
let firstURLPrism = Prism<[String],String>(
	tryGet: { $0.first },
	inject: { [$0] })
let imageURLPrism1 = infoDictPrism
	.join(imageDictPrism)
	.join(iconURLsPrism)
	.join(firstURLPrism)
```

Now we can `tryGet` the first URL with the following:

```swift
let firstURL = imageURLPrism1.tryGet(jsonExample1) /// this is "https://image.org/1.png"

let noFirstURL = imageURLPrism1.tryGet(jsonExample2) /// this is nil

let stillNoFirstURL = imageURLPrism1.tryGet(jsonExample3) /// this is nil
```

Our `imageURLPrism1` safely traverses the dictionary and extracts the desired information, if possible.

Now imagine a JSON example like this:

```swift
let jsonExample4 = [
	"info" : [
		"main_image_icon_url" : "https://image.org/3.png",
		"image" : [
			"icon_urls" : [
				"https://image.org/1.png",
				"https://image.org/2.png",
				"https://image.org/3.png"]]]]
```

The logic here is the following: for the icon image, search for the "main_image_icon_url" key: if it's there and it's a `String`, use that string as url; if it's not, fall back to the "icon_urls" array and get the first element. There are 2 possible paths now to get the same information, and that would require 2 prisms, but those should not be joined: *join* means *concatenate*, that is, apply the prisms in series, but what we want to do is applying them **in parallel**. That's what the `zip` function is about. Notice the signature (I omitted the "where" part):

```swift
func zip<OtherWhole, OtherPrism>(_ other: OtherPrism) -> Prism<(WholeType,OtherWhole),PartType>
```

The returned prism applies the two original prisms to 2 different "whole", but considers the same "part": even if the data structure is going the be the same for us, this is what we want, because the "part" is the same, it's just that two different prisms in general will *decompose* and *recompose* 2 different data structures, but the *isolated* component has to be the same. Let's define the new `imageURLPrism2`:

```swift
let imageURLPrism2 = infoDictPrism.join(anyDictPrism(at: "main_image_icon_url", as: String.self))
```

Finally, let's zip the prisms and apply them to `jsonExample4` and `jsonExample1`:

```swift
let finalURL1 = imageURLPrism2.zip(imageURLPrism1).tryGet(jsonExample4,jsonExample4) /// this is "https://image.org/3.png"
let finalURL2 = imageURLPrism2.zip(imageURLPrism1).tryGet(jsonExample1,jsonExample1) /// this is "https://image.org/1.png"

```

We got `"https://image.org/3.png"` as `finalURL1` because the first prism takes priority over the second one, and `finalURL2` is going to be `"https://image.org/1.png"` simply because there's no `"main_image_icon_url"`.

## Conclusion

That's it for prisms and lenses in Swift. Of course there are many more applications for both, but I hope I was able to make a case for using these constructs, that in my opinion help both clarity of code and maintainability. Some time investment has to be made to wrap our head around these abstractions, but after a while I can assure you it's going to be pretty natural to use them and more importantly to *think* in terms of them. 