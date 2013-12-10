{-# LANGUAGE FlexibleInstances #-}

{-|
  This module contains grouplike structures (structures based on @(S,op)@,
  where S is some set and @op@ is a binary operation on S) and the means to manipulate
  them.

  Unlike in the classical implementation via type classes, the algebraic structure
  is separated from the type of the elements and put into its own object, which
  is then passed to any function that wishes to exploit some algebraic property.
  To calculate @or@, for example, we can pass the monoid @(||, False)@ with a
  list of Bools to a sum function. To calculate @and@, we need only exchange
  the monoid to @(&&, True)@, while the list of Bools and the function to sum them
  stays exactly the same.

  Such a structure is parametrized with two types:

  1. the type of its elements (@el@) and

  2. a tag @t@ which can be used to identify it.

  The basic structure is the 'Magma', which doesn't guarantee anything
  beyond the presence of a binary operation on the structure's type.

  On top of this, a structure may posess certain additional traits. If we call
  the operation @+@ and the elements of the structure @a, b ,c ,x ,y , 1@ then tese are:

  [@Commutative@] @for all a, b: a + b = b + a@

  [@Associative@] @for all a, b, c: (a + b) + c = a + (b + c)@

  [@Idempotent@] @for all a: a + a = a@

  [@UnitElement@] @for all a and the unit element 1: a + 1 = 1 + a = a@

  [@LeftDivisible@] @for all a, b there exists x: x + a = b@

  [@RightDivisible@] @for all a, b there exists y: a + y = b@

  [@Invertible@] @for all a, their inverse a' and the unit element 1: a + a' = a' + a = 1@

  Structures with the desired traits be created directly through constructor
  methods (e.g. 'makeMonoid', which creates an associative structure with a
  unit element), or one can start off with 'makeMagma' and add the desired
  traits individually from there with the @add*@ methods.
  For example, the we can create the same monoid in two ways:

  @
    makeMonoid (+) 0 "AddMonoid" == addUnitElement 0 $ addAssociativity $ makeMagma (+) "AddMonoid"
  @

  Functions can specify that they expect a certain structure by giving a named
  structure and/or a collection of of desired traits in their context. A few examples:

  @
    sum :: (Monoid a) => a el t -> [el] -> el
    sum struct = foldl (op struct) (ident struct)
  @

  @
    removeDuplicates :: (Eq el, Associative a, Idempotent a) => a el t -> [el] -> [el]
    removeDuplicates _ = nub
  @

  Since the algebraic structures are just ordinary values, we can manipulate them as such.
  The following function takes a list of structures and gets their unit elements:

  @
    getUnits :: (Magma a) => [a el k] -> [el]
    getUnits = map ident
  @

  >>>getUnits [makeMonoid (+) 0 "add", makeSemigroup min "min", makeMonoid (*) 1 "mul", makeGroup (\x y -> 0) id 0 "zero"]
  [Just 0, Nothing, Just 1, Nothing]

  The following structures are predefined:

  1. Magma - closed under the operation; no other structure.

  2. Quasigroup - a magma with left and right dividers.

  3. Loop - a quasigroup with a unit element.

  4. Semigroup - an associative magma.

  5. Monoid - a semigroup with a unit element.

  6. Commutative monoid.

  7. Group - a magma which is both a monoid and a loop. Every element has an inverse.

  8. Commutative group.

  9. Semilattice - a commutative and idempotent semigroup. Its operation is commonly the meet or the join or two elements.

  10. Bounded semilattice - a semilattice with a unit element.
-}
module Grouplike (
   module Grouplike.Traits,
   GrouplikeStruct(gTag),

   -- * Predefined classes
   Quasigroup,
   Loop,
   Semigroup,
   Monoid,
   CommutativeMonoid,
   Group,
   CommutativeGroup,
   Semilattice,
   BoundedSemilattice,

   -- * Shorthand type synonyms for common structures
   MagmaStruct,
   QuasigroupStruct,
   LoopStruct,
   SemigroupStruct,
   MonoidStruct,
   CommutativeMonoidStruct,
   GroupStruct,
   CommutativeGroupStruct,
   SemilatticeStruct,
   BoundedSemilatticeStruct,

   -- * Constructors
   makeMagma,
   makeSemigroup,
   makeQuasigroup,
   makeLoop,
   makeMonoid,
   makeCommutativeMonoid,
   makeGroup,
   makeCommutativeGroup,
   makeSemilattice,
   makeBoundedSemilattice,

   -- * Adding traits to structures
   addCommutativity,
   addAssociativity,
   addIdempotence,
   addUnitElement,
   addLeftDivider,
   addRightDivider,
   addInverse
   ) where

import Grouplike.Traits
import Grouplike.Internal
