{-# LANGUAGE FlexibleInstances #-}

{-|
-}
module Relation.Internal where

import Helper
import Relation.Traits

class (Injective s, Functional s) => OneToOne s
class (Functional s, LeftTotal s) => Function s
class (Function s, Injective s, RightTotal s) => Bijection s

class (Reflexive s, Symmetric s, Transitive s) => Equivalence s
class (Reflexive s, Antisymmetric s, Transitive s) => PartialOrder s
class (PartialOrder s, Total s) => TotalOrder s

--well order: every non-empty set has a least element. ???

-- |A binary relation over sets X and Y, containing
--  elements of type el1 and el2, respectively.
data RelationStruct inj func lt rt el1 el2 =
   RelationStruct{relInjectivity::inj,
                  relFunctionality::func,
                  relLeftTotality::lt,
                  relRightTotality::rt,
                  relPred::Rel el1 el2}

-- |A binary endorelation, i.e. a relation
--  over one set X, containing elements of type el.
data EndorelationStruct' r s tr tot tri euc ser set rel el el2 =
   EndorelationStruct'{relReflexivity::r,
                      relSymmetry::s,
                      relTransitivity::tr,
                      relTotality::tot,
                      relTrichotonomity::tri,
                      relEuclidean::euc,
                      relSeriality::ser,
                      relSetLike::set,
                      relRelation::rel el el2}

type EndorelationStruct r s tr tot tri euc ser set rel el =
   EndorelationStruct' r s tr tot tri euc ser set rel el el

instance (InjectivityTag inj,
          FunctionalityTag func,
          LeftTotalityTag lt,
          RightTotalityTag rt) => Show (RelationStruct inj func lt rt el1 el2) where
   show struct = "Relation (" ++ implode ", " traits ++ ")"
      where i = getInjectivityValue $ relInjectivity struct
            f = getFunctionalityValue $ relFunctionality struct
            l = getLeftTotalityValue $ relLeftTotality struct
            r = getRightTotalityValue $ relRightTotality struct
            traits = filter (not . null) $ [show i, show f, show l, show r]


instance Relation (RelationStruct inj func lt rt) where
   rel = relPred

instance Relation (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct inj func lt rt)) where
   rel = relPred . relRelation
instance Endorelation (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct inj func lt rt)) where
   dummy = undefined

instance Injective (RelationStruct TagInjective func lt rt)
instance Functional (RelationStruct inj TagFunctional lt rt)
instance LeftTotal (RelationStruct inj func TagLeftTotal rt)
instance RightTotal (RelationStruct inj func lt TagRightTotal)

instance Injective (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct TagInjective func lt rt))
instance Functional (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct inj TagFunctional lt rt))
instance LeftTotal (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct inj func TagLeftTotal rt))
instance RightTotal (EndorelationStruct' r s tr tot tri euc ser set (RelationStruct inj func lt TagRightTotal))

instance Reflexive (EndorelationStruct' TagReflexive s tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Reflexive (EndorelationStruct' r TagSymmetric TagTransitive tot tri euc TagSerial set (RelationStruct inj func lt rt)) where
instance Irreflexive (EndorelationStruct' TagIrreflexive s tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Coreflexive (EndorelationStruct' TagCoreflexive s tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Symmetric (EndorelationStruct' r TagSymmetric tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Antisymmetric (EndorelationStruct' r TagAntisymmetric tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Asymmetric (EndorelationStruct' TagIrreflexive TagAntisymmetric tr tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Transitive (EndorelationStruct' r s TagTransitive tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Total (EndorelationStruct' r s tr TagTotal tri euc ser set (RelationStruct inj func lt rt)) where
instance Trichotonomous (EndorelationStruct' r s tr tot TagTrichotonomous euc ser set (RelationStruct inj func lt rt)) where
instance Euclidean (EndorelationStruct' r s tr tot tri TagEuclidean ser set (RelationStruct inj func lt rt)) where
instance Serial (EndorelationStruct' r s tr tot tri euc TagSerial set (RelationStruct inj func lt rt)) where
instance SetLike (EndorelationStruct' r s tr tot tri euc ser TagSetLike (RelationStruct inj func lt rt)) where

instance Equivalence (EndorelationStruct' TagReflexive TagSymmetric TagTransitive tot tri euc ser set (RelationStruct inj func lt rt)) where
instance Equivalence (EndorelationStruct' r TagSymmetric TagTransitive tot tri euc TagSerial set (RelationStruct inj func lt rt)) where
instance PartialOrder (EndorelationStruct' TagReflexive TagAntisymmetric TagTransitive tot tri euc ser set (RelationStruct inj func lt rt)) where
instance TotalOrder (EndorelationStruct' TagReflexive TagAntisymmetric TagTransitive TagTotal tri euc ser set (RelationStruct inj func lt rt)) where