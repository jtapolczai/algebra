{-# LANGUAGE TemplateHaskell #-}

{-|
  Contains primitive traits for grouplike algebraic structures. The existence of
  any trait is three-valued: it can be asserted (Commutativity, Associativity, etc.),
  explicitly denied (NoCommutativity, NoAssociativity, etc.), or left unspecified
  (UnknownCommutativity, UnknownAssociativity, etc.), which is the default case.

  For a traits, it three values are distinct types, but they implement
  the common interfaces 'PropertyTag' (for yes/no traits like commutativity) or
  'ElementTag' (for traits that has store some information, like 'UnitElement').

  The following traits exist:

  [@Commutative@] @for all a, b: a + b = b + a@

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

$(makeEnumTag "Commutativity" ["Commutative", "AntiCommutative", "NonCommutative", "UnknownCommutative"])
$(makeEnumTag "Associativity" ["Associative", "JacobiAssociative", "NonAssociative", "UnknownAssociative"])
$(makeEnumTag "Idempotence" ["Idempotent", "NonIdempotent", "UnknownIdempotent"])
$(makeContentTag "UnitElement" [("UnitElement", True), ("NoUnitElement", False), ("UnknownUnitElement", False)])
$(makeContentTag "LeftDivider" [("LeftDivider", True), ("NoLeftDivider", False), ("UnknownLeftDivider", False)])
$(makeContentTag "RightDivider" [("RightDivider", True), ("NoRightDivider", False), ("UnknownRightDivider", False)])
$(makeContentTag "Inverse" [("Inverse", True), ("NoInverse", False), ("UnknownInverse", False)])

-- |A grouplike structure with a binary operation.
class Grouplike s where
   op :: s el t -> Bin el

--Individual traits which compose into the known structures (Monoids, Groups, etc.)

-- |A commutative grouplike structure: a `op` b = b `op` a.
class Grouplike s => Commutative s where
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