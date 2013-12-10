module Relation.Traits where

import Traits
import Helper

-- Tags for the individual traits.

data Reflexivity = Reflexivity
data Irreflexivity = Irreflexivity
data UnknownReflexivity = UnknownReflexivity

instance PropertyTag Reflexivity where hasProperty _ = Just True
instance PropertyTag Irreflexivity where hasProperty _ = Just False
instance PropertyTag UnknownReflexivity where hasProperty _ = Just False

data 


data UnitElement a = UnitElement{getUI::a} deriving (Show, Eq, Read)
data NoUnitElement a = NoUnitElement deriving (Show, Eq, Read)
data UnknownUnitElement a = UnknownUnitElement deriving (Show, Eq, Read)

instance ElementTag UnitElement where getElement = Just .  getUI
instance ElementTag NoUnitElement where getElement _ = Nothing
instance ElementTag UnknownUnitElement where getElement _ = Nothing

data LeftDivider a = LeftDivider{getLD::a} deriving (Show, Eq, Read)
data NoLeftDivider a = NoLeftdivider deriving (Show, Eq, Read)
data UnknownLeftDivider a = UnknownLeftDivider deriving (Show, Eq, Read)

instance ElementTag LeftDivider where getElement = Just .  getLD
instance ElementTag NoLeftDivider where getElement _ = Nothing
instance ElementTag UnknownLeftDivider where getElement _ = Nothing

data RightDivider a = RightDivider{getRD::a} deriving (Show, Eq, Read)
data NoRightDivider a = NoRightDivider deriving (Show, Eq, Read)
data UnknownRightDivider a = UnknownRightDivider deriving (Show, Eq, Read)

instance ElementTag RightDivider where getElement = Just .  getRD
instance ElementTag NoRightDivider where getElement _ = Nothing
instance ElementTag UnknownRightDivider where getElement _ = Nothing

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