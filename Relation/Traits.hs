{-|
  Contains primitive traits for binary (endo)relations. The existence of
  any trait is three-valued: it can be asserted (Commutativity, Associativity, etc.),
  explicitly denied (NoCommutativity, NoAssociativity, etc.), or left unspecified
  (UnknownCommutativity, UnknownAssociativity, etc.), which is the default case.

  For a traits, it three values are distinct types, but they implement
  the common interfaces 'PropertyTag' (for yes/no traits like commutativity) or
  'ElementTag' (for traits that has store some information, like 'UnitElement').

  The following traits exist:

  TODO
-}
module Relation.Traits where

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

import Templates
import Helper

-- Tags for the individual traits of binary relations.

$(makeEnumTag "Injectivity" ["Injective", "NonInjective", "UnknownInjective"])
$(makeEnumTag "Functionality" ["Functional", "NonFunctional", "UnknownFunctional"])
$(makeEnumTag "LeftTotality" ["LeftTotal", "NonLeftTotal", "UnknownLeftTotal"])
$(makeEnumTag "RightTotality" ["RightTotal", "NonRightTotal", "UnknownRightTotal"])

-- Tags for the individual traits of binary endorelations.
$(makeEnumTag "Reflexivity" ["Reflexive", "Irreflexive", "Coreflexive", "NonReflexive", "UnknownReflexive"])
$(makeEnumTag "Symmetry" ["Symmetric", "Antisymmetric", "NonSymmetric", "UnknownSymmetric"])
$(makeEnumTag "Transitivity" ["Transitive", "NonTransitive", "UnknownTransitive"])
$(makeEnumTag "Totality" ["Total", "NonTotal", "UnknownTotal"])
$(makeEnumTag "Trichotonimity" ["Trichotonomous", "NonTrichotonomous", "UnknownTrichotonomous"])
$(makeEnumTag "Euclidean" ["Euclidean", "NonEuclidean", "UnknownEuclidean"])
$(makeEnumTag "Seriality" ["Serial", "NonSerial", "UnknownSerial"])
$(makeEnumTag "SetLike" ["SetLike", "NonSetLike", "UnknownSetLike"])

instance Show InjectivityValue where
   show UnknownInjective = ""
   show Injective = "Injective"
   show NonInjective = "NonInjective"

instance Show FunctionalityValue where
   show UnknownFunctional = ""
   show Functional = "Functional"
   show NonFunctional = "NonFunctional"

instance Show LeftTotalityValue where
   show UnknownLeftTotal = ""
   show LeftTotal = "LeftTotal"
   show NonLeftTotal = "NonLeftTotal"

instance Show RightTotalityValue where
   show UnknownRightTotal = ""
   show RightTotal = "RightTotal"
   show NonRightTotal = "NonRightTotal"

instance Show ReflexivityValue where
   show UnknownReflexive = ""
   show Reflexive = "Reflexive"
   show Irreflexive = "Irreflexive"
   show Coreflexive = "Coreflexive"
   show NonReflexive = "NonReflexive"

instance Show SymmetryValue where
   show UnknownSymmetric = ""
   show Symmetric = "Symmetric"
   show Antisymmetric = "Antisymmetric"
   show NonSymmetric = "NonSymmetric"

instance Show TransitivityValue where
   show UnknownTransitive = ""
   show Transitive = "Transitive"
   show NonTransitive = "NonTransitive"

instance Show TotalityValue where
   show UnknownTotal = ""
   show Total = "Total"
   show NonTotal = "NonTotal"

instance Show TrichotonimityValue where
   show UnknownTrichotonomous = ""
   show Trichotonomous = "Trichotonomous"
   show NonTrichotonomous = "NonTrichotonomous"

instance Show EuclideanValue where
   show UnknownEuclidean = ""
   show Euclidean = "Euclidean"
   show NonEuclidean = "NonEuclidean"

instance Show SerialityValue where
   show UnknownSerial = ""
   show Serial = "Serial"
   show NonSerial = "NonSerial"

instance Show SetLikeValue where
   show UnknownSetLike = ""
   show SetLike = "SetLike"
   show NonSetLike = "NonSetLike"

-- |A relation with a binary predicate.
class Relation s where
   rel :: s el1 el2 -> Rel el1 el2

--TODO: add a kind signature to this to
--get rid of dummy.
-- |An endorelation with a binary predicate.
class Relation s => Endorelation s where
   dummy :: s el el -> Rel el el


-- |An injective relation: aRc and bRc implies a = b.
class Relation s => Injective s
-- |A functional relation: aRb and aRc implies b = c.
class Relation s => Functional s
-- |A left-total relation: for all a there is b s.t. aRb.
class Relation s => LeftTotal s
-- |A right-total (surjective) relation: for all b there is a s.t. aRb.
class Relation s => RightTotal s

-- |A reflexive relation: aRa.
class Endorelation s => Reflexive s
-- |An irreflexive relation: not aRa.
class Endorelation s => Irreflexive s
-- |A coreflexive relation: aRb implies a = b.
class Endorelation s => Coreflexive s
-- |A symmetric relation: aRb implies bRa.
class Endorelation s => Symmetric s
-- |An antisymmetric relation: aRb and bRa implies a = b.
class Endorelation s => Antisymmetric s
-- |An asymmetric relation: aRb implies not bRa.
--  a relation is asymmetric iff it is antisymmetric and irreflexive.
class (Antisymmetric s, Irreflexive s) => Asymmetric s
-- |A transitive relation: aRb and bRc implies aRc.
class Endorelation s => Transitive s
-- |A total relation: for all a,b: aRb or bRa.
class Endorelation s => Total s
-- |A trichotonomous relation: for all a,b exactly one of the following holds:
--
--  * aRb
--  * bRa
--  * a = b
class Endorelation s => Trichotonomous s
-- |An Euclidean relation: for all a,b,c: aRb and aRc implies bRc and cRb.
class Endorelation s => Euclidean s
-- |A serial relation: for all a there exists b s.t. aRb.
class Endorelation s => Serial s
-- |A set-like relation: for all a: the class of all b s.t. 
class Endorelation s => SetLike s
