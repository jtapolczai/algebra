{-# LANGUAGE TemplateHaskell #-}

{-|
  Contains primitive traits for grouplike algebraic structures. The existence of
  is generally True/False, but more than two values are also possible, and
  some values may carry values or functions as "payload": a "unit element" trait
  might, for example, contain the value of the unit element, and a "uniquely factorizable"
  trait might carry a factorization function.

  For any trait, its values are distinct types, but each is an instance
  of a class which provides a method for converting these values into
  a common sum type. This sum type can then be used to perform reflection
  about the traits of an algebraic structure at runtime, or to put
  algebraic structures with different traits into the same collection.

  The following traits exist:

  [@Commutative@] Commutative: @for all a, b: a + b = b + a@. AntiCommutative:
  @for all a,b and the inverse function ': a+b = (b+a)'@.

  [@Associative@] @for all a, b, c: (a + b) + c = a + (b + c)@

  [@Idempotent@] @for all a: a + a = a@

  [@UnitElement@] @for all a and the unit element 1: a + 1 = 1 + a = a@

  [@LeftDivisible@] @for all a, b there exists x: x + a = b@

  [@RightDivisible@] @for all a, b there exists y: a + y = b@

  [@Invertible@] @for all a, their inverse a' and the unit element 1: a + a' = a' + a = 1@

-}
module Grouplike.Traits where

import Helper
import Templates

--non-sum types:
-- TagCommutative = TagCommutative
--sum types:
-- CommutativityValue = Commutative | ...
--classes:
-- Commutative

-- Tags for the individual traits.

-- Every individual trait consists of:
-- 1. The sum type of its values.
-- 2. The NON-SUM type of its values, which exist to separate structures
--    with different values for a trait in the eyes of the type checker.
-- 3. A class which transforms the non-sum type into the sum type for dynamic checking.
--
-- A tag may be a simple enum (Commutative | NonCommutative | UnknownCommutative), or
-- it may have a "payload", for example a unit element, which will exist in
-- some cases, i.e.: UnitElement a | NoUnitElement | UnknownElement

-- Simple enums can be made with the macro @makeEnumTag@. Tags with payload are made with @makeContentTag@,
-- where each option has type (String, Bool). True in the second component indicates the existence of data
-- for that particular constructor.

$(makeEnumTag "Commutativity" ["Commutative", "AntiCommutative", "UnknownCommutative"])
$(makeEnumTag "Associativity" ["Associative", "UnknownAssociative"])
$(makeEnumTag "Idempotence" ["Idempotent", "UnknownIdempotent"])
$(makeContentTag "UnitElement" [("UnitElement", True), ("UnknownUnitElement", False)])
$(makeContentTag "LeftDivider" [("LeftDivider", True), ("UnknownLeftDivider", False)])
$(makeContentTag "RightDivider" [("RightDivider", True), ("UnknownRightDivider", False)])
$(makeContentTag "Inverse" [("Inverse", True), ("UnknownInverse", False)])

grouplikeTraits = ["Commutative", "Associative", "Idempotent", "UnitElement",
                   "LeftDivider", "RightDivider", "Inverse"]

instance Show CommutativityValue where
   show UnknownCommutative = ""
   show Commutative = "Commutative"
   show AntiCommutative = "AntiCommutative"

instance Show AssociativityValue where
   show UnknownAssociative = ""
   show Associative = "Associative"

instance Show IdempotenceValue where
   show UnknownIdempotent = ""
   show Idempotent = "Idempotent"

instance Show el => Show (UnitElementValue el) where
   show UnknownUnitElement = ""
   show (UnitElement el) = "UnitElement " ++ show el

instance Show (LeftDividerValue el) where
   show UnknownLeftDivider = ""
   show (LeftDivider el) = "LeftDivider"

instance Show (RightDividerValue el) where
   show UnknownRightDivider = ""
   show (RightDivider el) = "RightDivider"

instance Show (InverseValue el) where
   show UnknownInverse = ""
   show (Inverse el) = "Inverse"

-- |A grouplike structure with a binary operation.
class Grouplike s where
   op :: s el t -> Bin el

--Individual traits which compose into the known structures (Monoids, Groups, etc.)

-- |A commutative grouplike structure: a `op` b = b `op` a.
class Grouplike s => Commutative s where
-- |An anticommutative grouplike structure: a `op` b = inv (b `op` a).
class Grouplike s => Anticommutative s where
-- |A associative grouplike structure: (a `op` b) `op` c = a `op` (b `op` c).
class Grouplike s => Associative s where
-- |An idempotent grouplike structure: x `op` x = x
class Grouplike s => Idempotent s where
-- |A grouplike structure with a unit element U: a `op` U = U `op` a = a 
class Grouplike s => HasUnitElement s where
   ident :: s el t -> el
-- |A grouplike structure where there exists a left divider l for
--  every pair of elements a,b: a `op` l = b
class Grouplike s => LeftDivisible s where
   lDiv :: s el t -> Bin el
-- |A grouplike structure where there exists a right divider r for
--  every pair of elements a,b: r `op` a = b
class Grouplike s => RightDivisible s where
   rDiv :: s el t -> Bin el
-- |A grouplike structure which has both left and right dividers.
class (Grouplike s, LeftDivisible s, RightDivisible s) => Divisible s where
-- |A grouplike structure where every element a has an inverse ia,
--  which reduces a to the unit element U: a `op` ia = U
class (Grouplike s, HasUnitElement s) => Invertible s where
   inverse :: s el t -> Un el
