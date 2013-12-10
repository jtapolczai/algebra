
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

import Helper


-- |A property tag for a structure which can be set to yes/no/unknown,
--  corresponding to Just True/Just False/Nothing, respectively.
class PropertyTag t where
   --Returns the yes/no/unknown-value to which the tag corresponds.
   hasProperty :: t -> Maybe Bool
-- |An element tag for a structure, where the element may or may not be present.
--  An example would be a the unitElement, which is Nothing for a semigroup,
--  but Just 0 for the monoid (N,+,0).
class ElementTag t where
   --Returns the Just content of the tag if it has any content and Nothing otherwise.
   getElement :: t el -> Maybe el 

-- Tags for the individual traits.

data Commutativity = Commutativity deriving (Show, Eq, Read)
data NoCommutativity = NoCommutativity deriving (Show, Eq, Read)
data UnknownCommutativity = UnknownCommutativity deriving (Show, Eq, Read)

instance PropertyTag Commutativity where hasProperty _ = Just True
instance PropertyTag NoCommutativity where hasProperty _ = Just False
instance PropertyTag UnknownCommutativity where hasProperty _ = Nothing


data Inverse a = Inverse{getInv::a} deriving (Show, Eq, Read)
data NoInverse a = NoInverse deriving (Show, Eq, Read)
data UnknownInverse a = UnknownInverse deriving (Show, Eq, Read)

instance ElementTag Inverse where getElement = Just .  getInv
instance ElementTag NoInverse where getElement _ = Nothing
instance ElementTag UnknownInverse where getElement _ = Nothing

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