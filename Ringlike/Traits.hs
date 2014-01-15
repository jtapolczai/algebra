{-# LANGUAGE KindSignatures #-}

{-|
  Contains primitive traits for ringlike algebraic structures. The existence of
  is generally True/False, but more than two values are also possible, and
  some values may carry values or functions as "payload": a "unit element" trait
  might, for example, contain the value of the unit element, and a "uniquely factorizable"
  trait might carry a factorization function.

  For any trait, its values are distinct types, but each is an instance
  of a class which provides a method for converting these values into
  a common sum type. This sum type can then be used to perform reflection
  about the traits of an algebraic structure at runtime, or to put
  algebraic structures with different traits into the same collection.

  Ringlike structure provide two operations, called @op1@ and @op2@. For
  readability, we will refer to them as @+@ and @*@. The unit elements
  of @+@ and @*@, if they exist, will be called @0@ and @1@.

  The following traits exist:

  [@LeftDistributivity@] @for all a, b, c: a * (b + c) = a*c + b*c@.

  [@RightDistributivity@] @for all a, b, c: (a + b) * c = a*c + b*c@.

  [@LeftAnnihilation@] @0@ annihilates @*@ from the left: @for all a: a * 0 = 0@.

  [@RightAnnihilation@] @0@ annihilates @*@ from the left: @for all a: a * 0 = 0@.


  [@Annihilation@] LeftAnnihilating: @0@ annihilates @*@ from the left: @for all a: a * 0 = 0@.
  RightAnnihilating: @0@ annihilates @*@ from the right: @for all a: 0 * a = 0@. Annihilating: both
  left- and right-annihilation hold, i.e. @for all a: a * 0 = 0 * a = 0@.

  [@JacobiIdentity@] The Jacobi Identity is satisfied: @for all a, b, c: (a * (b * c)) + (b * (c * a)) + (c * (a * b)) = 0@.

  [@Eliminating@] @0@ annihilates @*@: @for all a: a * a = 0@.

  [@ZeroProduct@] @forall a, b: if a*b = 0@, then @a@ or @b@ equals @0@. 

  [@PositiveProduct@] The product of two non-zero elements is non-zero:
  @forall a, b: (a /= 0 && b /= 0)@ implies @a * b /= 0@.

  [@UniquelyFactorizable@] Every element has a unique factorization @[(a_i,e_i),...,(a_n,e_n)] :: [(a,Int)]@ into prime elements.
  @forall a: a = foldl1 (*) $ concatMap pow $ [(a_i,e_i),...,(a_n,e_n)] where pow (a,e) = replicate e a@.

  [@Euclidean@] A Euclidean function @f :: el -> Int@ can be used. @forall a, b: (a /= 0) && (a /= b) implies
  that there exist q, r s.t. a = b*q + r@ and @r = 0 || f(r) < f(r)@.

  [@Absorbing@] @for all a, b: a + (a * b) = a = a * (a + b)@.

  [@Complement@] Every element has a complement: @a + (complement a) = 1@ and
  @a * (complement a) = 0@. In the context of Boolean algebras, specifically, the complement
  is @not@.
-}
module Ringlike.Traits where

import Grouplike.Internal
import Helper
import Templates


$(makeEnumTag "LeftDistributivity" ["LeftDistributive", "UnknownLeftDistributive"])
$(makeEnumTag "RightDistributivity" ["RightDistributive", "UnknownRightDistributive"])
$(makeEnumTag "Annihilation" ["Annihilating", "LeftAnnihilating", "RightAnnihilating", "UnknownAnnihilating"])
$(makeEnumTag "JacobiIdentity" ["JacobiIdentity", "UnknownJacobiIdentity"])
$(makeEnumTag "Eliminating" ["Eliminating", "UnknownEliminating"])
$(makeEnumTag "ZeroProduct" ["ZeroProduct", "UnknownZeroProduct"])
$(makeEnumTag "PositiveProduct" ["PositiveProduct", "UnknownPositiveProduct"])
$(makeContentTag "UniquelyFactorizable" [("UniquelyFactorizable", True), ("UnknownUniquelyFactorizable", False)])
$(makeEnumTag "Euclidean" ["Euclidean", "UnknownEuclidean"])
$(makeEnumTag "Absorbing" ["Absorbing", "UnknownAbsorbing"])
$(makeContentTag "Complement" [("Complement", True), ("UnknownComplement", False)])

ringlikeTraits = ["LeftDistributive", "RightDistributive", "Annihilating",
                  "JacobiIdentity", "Eliminating", "ZeroProduct", "PositiveProduct",
                  "UniquelyFactorizable", "Euclidean", "Absorbing", "Complement"]

instance Show LeftDistributivityValue where
   show UnknownLeftDistributive = ""
   show LeftDistributive = "LeftDistributive"

instance Show RightDistributivityValue where
   show UnknownRightDistributive = ""
   show RightDistributive = "RightDistributive"

instance Show AnnihilationValue where
   show UnknownAnnihilating = ""
   show Annihilating = "Annihilating"
   show LeftAnnihilating = "LeftAnnihilating"
   show RightAnnihilating = "RightAnnihilating"

instance Show JacobiIdentityValue where
   show UnknownJacobiIdentity = ""
   show JacobiIdentity = "JacobiIdentity"

instance Show EliminatingValue where
   show UnknownEliminating = ""
   show Eliminating = "Eliminating"

instance Show ZeroProductValue where
   show UnknownZeroProduct = ""
   show ZeroProduct = "ZeroProduct"

instance Show PositiveProductValue where
   show UnknownPositiveProduct = ""
   show PositiveProduct = "PositiveProduct"

instance Show (UniquelyFactorizableValue f) where
   show UnknownUniquelyFactorizable = ""
   show (UniquelyFactorizable _) = "UniquelyFactorizable"

instance Show EuclideanValue where
   show UnknownEuclidean = ""
   show Euclidean = "Euclidean"

instance Show AbsorbingValue where
   show UnknownAbsorbing = ""
   show Absorbing = "Absorbing"

instance Show (ComplementValue c) where
   show UnknownComplement = ""
   show (Complement _) = "Complement"

-- |A grouplike structure composed of two grouplike structures.
class Ringlike r where
   getStruct1 :: Grouplike g1 => r g1 g2 el t1 t2 -> (g1 el t1)
   getStruct2 :: Grouplike g2 => r g1 g2 el t1 t2 -> (g2 el t2)

--Individual traits which compose into the known structures (Rigs, Rings, Rngs, etc.)

-- |A ringlike structure with left-distributivity.
class Ringlike s => LeftDistributive s where
-- |A ringlike structure with right-distributivity.
class Ringlike s => RightDistributive s where
-- |A ringlike structure with both and left- and right-divisibility.
class (LeftDistributive s, RightDistributive s) => Distributive s where
-- |An annihilating ringlike structure.
class Ringlike s => Annihilating s where
-- |A ringlike structure which satisfies the Jacobi identity.
class Ringlike s => JacobiIdentity s where
-- |A ringlike structure in which multiplication of an element with itself result in the
--  additive unit
class Ringlike s => Eliminating s where
-- |A ringlike structure in which @a*b = 0@ implies @a@ or @b@ to be @0@.
class Ringlike s => ZeroProduct s where
-- |A ringlike structure in which the product of two non-zero elements is non-zero.
class Ringlike s => PositiveProduct s where
-- |A ringlike structure in which every element can be uniquely factorized.
class Ringlike s => UniquelyFactorizable s where
  factorize :: s g1 g2 el t1 t2 -> el -> [(el, Int)]
-- |A ringlike structure in which a Euclidean function can be used.
class Ringlike s => Euclidean s where
-- |A ringlike structure where absorption holds.
class Ringlike s => Absorbing s where
-- |A ringlike structure in which every element has a complement.
class Ringlike s => Complement s where
  complement :: s g1 g2 el t1 t2 -> el -> el