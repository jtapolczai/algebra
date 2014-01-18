algebra
=======
***NOTE: This package is very much experimental / a work in progress! Use at your own peril, ye who thread here.***

Provides common, customizable algebraic structures (grouplike & ringlike, relations, modules, vector spaces) and operations on them in Haskell. The algebraic structures, instead of being immaterial type classes that types like `Int` and `Bool` implement, are themselves objects that can be passed around, manipulated, and switched out when desired. In addition to predefined structures like monoids and mings, the user can also create his own by mixing and matching from list of predefined **traits** like `Commutative`, `Distributive`, `Idempotent`, etc.

Features
--------

The package provides the following classes of structures:
* **Grouplike structures**. Predefined are:
  * magmas
  * quasigroups
  * semigroups
  * loops
  * (commutative) monoids
  * (commutative) groups
  * (bounded) semilattices
* **Ringlike structures**. Predifined are:
  * semirings
  * rigs
  * (right/left) near rings
  * (commutative) rings
  * (bounded) lattices
  * Boolean lattices
  * domains
  * integral domains
  * unique factorization domains
  * Euclidean domains
  * fields
* **Relations**. Predefined are:
  * partial/total orderings
  * equivalence relations
* **Modules**. to be done
* **Vector spaces**. to be done
* **Algebras**. to be done
* **Automata theory**. to be done

In each category, more structures can be defined by the user by selecting combinations of predefined traits --- in fact, the predefined structures are just there for convenience and are nothing more than shorthand notation for the presence of trait-combinations: every structure that is associative and has a unit element is automatically a monoid, every one which is invertible is also a group. Any function can ask for specific, named structures, or just certain traits.

Every algegbraic structure is a *value* and can be created and passed around in the ordinary fashion. Lists of such structures can be given to functions, and arbitrarily many algebraic structures can exist for the same type.

Structure objects
-----------------

The standard technique for implemented algebraic properties is to declare a type class (say, `Monoid`) and then have a type like `Int` implement it. This is fine most of the time, but has several drawbacks:
* A single type may only implement the structure once. For `Int`, both `(Int, +, 1)` and `(Int, *, 0)` would be suitable monoids, be we are forced to arbitrarily choose one of them.
* This restriction is idiomatically circumvented by introducing `newtypes`: if we "used up" the monoid instance for `Int` on the addition, we can create the **wrapper** `newtype MultInt = MI{fromMI::Int}`
and then have that implement the multiplicative monoid. While viable, this is laborious in even the simplest cases, and gets only worse if the `Int`s in question are tucked away inside all sorts of complicated data structures --- then, we are forced to unpack and re-pack them, (un-)wrapping `newtype` declarations just to change which monoid gets used. 
* The problem is further exacerbated when we try to compose algebraic structures --- two (commutative) monoids can be combined to make a semiring, but since two monoid implementations necessitate the two nominally different types `Int` and `MultInt`, the operations and the unit elements become incompatible. Whenever we want to apply `(*)`, we have to wrap borth operands inside a `MultInt`; whenever we want to use `(+)`, we have to unwrap them. The same goes for `0` and `1`, even though they are both supposed to be `Int`s.

To solve these problems, this library turns algebraic structures into simple values, independent of the types on which they operate. Due to this, a single type may have multiple structures of the same kind associated with it instead of just one --- if we want to, say, have both the additive monoid `(Int, +, 1)` and the multiplicative monoid `(Int, *, 0)` for `Int`, we can simply create two objects and pass them to functions which want to exploit the monoidal structure, while the type `Int` stays completely oblivious. If we want to create a semiring (or any other structure) out of these two, we simply put them together and get a value which contains information about both.


### Motivating example #1

Suppose we wished to write the following function (`mappend` being the operation of a monoid, and `mempty` its unit element):
```haskell
alternate [x_1,...,x_n] = x_1 + x_2 * x_3 + x_4 * ... * x_n
```

`foldl mappend mempty` wouldn't work because it wouldn't alternate between `(+)` and `(*)`. We could try to convert every other elemet into a `MultInt`, but this would create a heterogeneous list, which is illegal in Haskell. We instead have to extract `MultInt`'s `mappend` function:
```haskell
alternate = foldl f (mempty::Int, opList)
  where mult a@(MI _) b@(MI _) = c
          where (MI c) = a `mappend` b 
        plus :: Int -> Int -> Int
        plus = mappend
        opList = plus : mult : opList
        
        f (acc, op:ops) value = (acc `op` value, ops)
```

Now, however, `alternate` is no longer truly polymorphic: it only works for integers, but not for booleans, for which we could replace `+` and `*` with `||` and `&&`. To get back polymorphism over all monoids, we'd have to pass along a dummy value for `MultInt` (to get its `mappend`) and the extracting function ``fromMI`:
```haskell
alternate :: (Monoid a, Monoid b) => b -> (b -> a) -> [a] -> a
alternate dummy extract = foldl f (mempty, opList)
  where mult a b = extract c
          where c = a `mappend` b 
        opList = mappend : mult : opList
        
        f (acc, op:ops) value = (acc `op` value, ops)
```

#### With value-level algebraic structures

We could much simplify the whole affair by splitting off algebraic structures into independent values and thus declutter `Int`. That would enable us to write the function thus (`op` is the operation, `ident` the unit element):
```haskell
alternate :: (Monoid a , Monoid b) => (a el) -> (b el) -> [el]
alternate = foldl f (ident a, opList)
  where opList = (op a) : (op b) : opList
        f (acc, op:ops) value = (acc `op` value, ops)
```

This version is fully polymorophic and requires only the obvious values: two monoid structures over `Int` to tell it what the operations and the units are, and a list of `Int`s. This, no more `newtypes` and casting to and fro are needed.

### Motivating example #2

Suppose that we wish to take two (commutative) monoids and, knowing that distributivity holds and that "multiplication" with 0 results in 0, create a semiring. If the two monoids are type classes over two types, there is no obvious way to do this. We'd either have to write a `Semiring` class from scratch,

```haskell
class Semiring a where
  plus :: a -> a -> a
  mult :: a -> a -> a
  zero :: a
  one :: a
```

or somehow use a dummy value of the second monoid to get its operation

```haskell
class (Monoid a, Monoid b) => Semiring a b where
  plus :: a -> a -> a
  mult :: b -> (b -> a) a -> a -> a
  zero :: a
  one :: b -> (b -> a) -> a
```

For `Int`, calling `mult` would look like this: `mult (MultInt 0) fromMI 3 7 = 21`.

We can do this much more easily if monoid are just values; we could just have a type `Semiring` and a function
```haskell
-- Create a semiring R out of two monoids, where (*) distributes over (+) and where (*0) annihilates R.
makeSemiring :: (Monoid a, Monoid b) => (a e) -> (b e) -> Semiring e
makeSemiring a b = addAnnihilation $ addDistributivity  $ makeBasicRinglike a b
```

To get back the constituting monoids, we could call simply call two getters `struct1` and `struct2`.

Dynamic & static structures
---------------------------

Per default, constraints like `Commutative` on algebraic structures are statically enforced and structures with different traits have different types. As an example:

```haskell
solveFormula :: (Field f s1 s2) => f s1 s2 el -> [[el]] -> el
solveFormula f = sum . map product
   where product = foldl (op $ getStruct1 f) (ident $ getStruct1 f)
         sum = foldl (op $ getStruct2 f) (ident $ getStruct2 f)
```

Here, we want to calculate the result of an expression of the form <img src="http://latex.codecogs.com/gif.latex?\inline&space;(a_{1,1}&space;*&space;\dots&space;*&space;a_{1,n_1})&space;&plus;&space;\dots&space;&plus;&space;(a_{m,1}&space;*&space;\dots&space;*&space;a_{m,n_m})" title="(a_{1,1} * \dots * a_{1,n_1}) + \dots + (a_{m,1} * \dots * a_{m,n_m})" /> and specify that we want as the first argument a field composed of two substructures `s1` and `s2`. The type definition of a field ensures that `s1` and `s2` will be commutative groups. We get the functions `sum` and `product` by simply extracting the operations and unit elements of `s1` and `s2`.

For most applications, this completely suffices and will be appropriate. However, there are certain things that cannot be done in this framework. Specifically, one cannot
* accept an arbitrary structure and determine its traits dynamically or
* accept a list of different structures.

There is a way to accomplish these tasks: every kind of structure has an `Internal` module which exposes details about its construction and allows the bypassing of the static type checks. The internal interface for, say, grouplike structures, is simply an expanded version of the default one, with two additions:
1. for every traits, there is an accessor named `isCommutative`, `hasUnitElement`, `isDistributive`, or something similar. These functions can be applied on any structure to get information about it at runtime and
2. the function `makeDynamic` which, when applied to a structure, makes it **dynamic**. All dynamic structures (within a category) are of the same type; thereby, they bypass all static type checks on structures and will be accepted everywhere.

`makeDynamic` is of course hazardrous, but can sometime be useful. For example, we can write a function that goes through a list of grouplike structures and determines whether they are commutative:

```haskell
intAdd = makeCommutativeGroup (+) 0
intSub = makeMonoid (-) 0
digitConcat = makeSemigroup (\x y -> read $ show x ++ show y) 

mapCommutative :: [DynamicGroupStruct el] -> [CommutativityValue]
mapCommutative = map (f . isCommutative)
  where f Commutative = True
        f _           = False -- 'UnknownCommutative' and 'AntiCommutative' are also possible
```

Testing this out:

```haskell
> import Grouplike.Internal (makeDynamic)
> mapCommutative [makeDynamic intAdd, makeDynamic intSub, makeDynamic digitConcat]
[True, False, False]
```
