{-# LANGUAGE KindSignatures #-}

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
module Ringlike.Traits where

import Grouplike.Internal
import Helper
import Templates


$(makeEnumTag "LeftDistributivity" ["LeftDistributive", "NonLeftDistributive", "UnknownLeftDistributive"])
$(makeEnumTag "RightDistributivity" ["RightDistributive", "NonRightDistributive", "UnknownRightDistributive"])
$(makeEnumTag "Annihilation" ["Annihilating", "NonAnnihilating", "UnknownAnnihilating", "AllAnnihilating"])
$(makeEnumTag "JacobiIdentity" ["JacobiIdentity", "NoJacobiIdentity", "UnknownJacobiIdentity"])
$(makeEnumTag "Bilinearity" ["Bilinear", "NoBilinear", "UnknownBilinear"])
$(makeEnumTag "Eliminating" ["Eliminating", "NoEliminating", "UnknownEliminating"])

-- $(makeEnumTag "" ["", "No", "Unknown"])



instance Show LeftDistributivityValue where
   show UnknownLeftDistributive = ""
   show LeftDistributive = "LeftDistributive"
   show NonLeftDistributive = "NonLeftDistributive"

instance Show RightDistributivityValue where
   show UnknownRightDistributive = ""
   show RightDistributive = "RightDistributive"
   show NonRightDistributive = "NonRightDistributive"

instance Show AnnihilationValue where
   show UnknownAnnihilating = ""
   show Annihilating = "Annihilating"
   show NonAnnihilating = "NonAnnihilating"
   show AllAnnihilating = "Great Master Cthulhu"

instance Show JacobiIdentityValue where
   show UnknownJacobiIdentity = ""
   show JacobiIdentity = "JacobiIdentity"
   show NoJacobiIdentity = "NoJacobiIdentity"

instance Show BilinearityValue where
   show UnknownBilinear = ""
   show Bilinear = "Bilinear"
   show NoBilinear = "NoBilinear"

instance Show EliminatingValue where
   show UnknownEliminating = ""
   show Eliminating = "Eliminating"
   show NoEliminating = "NoEliminating"

-- |A grouplike structure composed of two grouplike structures.
class Ringlike r where
   getStruct1 :: Grouplike g1 => r g1 g2 el t1 t2 -> (g1 el t1)
   getStruct2 :: Grouplike g2 => r g1 g2 el t1 t2 -> (g2 el t2)

--Individual traits which compose into the known structures (Rigs, Rings, Rngs, etc.)

-- |A ringlike structure with left-distributivity: @a `op2` (b `op1` c) = (a `op2` b) `op1` (a `op2` c)@
class Ringlike s => LeftDistributive s where
-- |A ringlike structure with right-distributivity: @(b `op1` c) `op2` a = (b `op2` a) `op1` (c `op2` a)@
class Ringlike s => RightDistributive s where
-- |A ringlike structure with both and left- and right-divisibility.
class (LeftDistributive s, RightDistributive s) => Distributive s where
-- |A ringlike structure where the unit element the unit element of the first structure (@0@) annihilates the operation of the second (@*@), i.e.
--  @0 * x = 0 * x = 0@
class Ringlike s => Annihilating s where
-- |A ringlike structure which satisfies the Jacobi identity:
--  @(a `op2` (b `op2` c)) `op1` (b `op2` (c `op2` a)) `op1` (c `op2` (a `op2` b)) = 0@
class Ringlike s => JacobiIdentity s where
-- |A ringlike structure with bilinearity: @(a `op1` b) `op2` c = (a `op2` c) `op1` (b `op2` c)@
class Ringlike s => Bilinear s where
-- |A ringlike structure in which it holds that @a `op2` a = 0@.
--  Note: 0 is the unit element of the FIRST operation.
class Ringlike s => Eliminating s where