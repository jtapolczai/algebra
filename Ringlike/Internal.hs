{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}

module Ringlike.Internal where

import Grouplike.Internal hiding (makeDynamic)
import qualified Grouplike.Internal as G
import Ringlike.Traits
import Helper
import Templates

-- |A lattice with join and meet.
class (Semilattice a, Semilattice b, Distributive r, Annihilating r) => Lattice r a b
-- |A bounded lattice with greatest and smallest elements.
class (BoundedSemilattice a, BoundedSemilattice b, Lattice r a b) => BoundedLattice r a b
-- |A Boolean ring, composed of two semilattices. Every element has a complement and absorption holds.
class (Lattice r a b, Ring r a b) => BooleanRing r a b

-- |A semiring, composed of a commutative additive monoid and a multiplicative monoid. 
class (CommutativeMonoid a, Monoid b, Distributive r, Annihilating r) => Semiring r a b
-- |A right near-ring where the second operation does not have a unit element and only right-distributivity holds.
class (Group a, Semigroup b, RightDistributive r) => RightNearRing r a b
-- |A left near-ring where the second operation does not have a unit element and only left-distributivity holds.
class (Group a, Semigroup b, LeftDistributive r) => LeftNearRing r a b
-- |A ring, composed of a commutative group and a monoid.
class (CommutativeGroup a, Semiring r a b) => Ring r a b
-- |A ring where a * b = 0 implies that a or b are 0.
class (Ring r a b, ZeroProduct r) => Domain r a b
-- |A commutative ring: both operations are commutative.
class (CommutativeMonoid b, Ring r a b) => CommutativeRing r a b
-- |An integral domain: a commutative ring where the product of two non-zero elements is non-zero.
class (CommutativeRing r a b, PositiveProduct r) => IntegralDomain r a b
-- |A unique factorization domain: every element can be uniquely decomposed into irreducible (prime) elements.
class (IntegralDomain r a b, UniquelyFactorizable r) => UniqueFactorizationDomain r a b where
-- |An Euclidean domain: the Eudclidean algorithm can be applied to find the greatest common divisor of two elements.
class (IntegralDomain r a b) => EuclideanDomain r a b where
-- |A field: every element has an additive and a multiplicative inverse, i.e. both components commutative groups.
class (EuclideanDomain r a b, CommutativeGroup b) => Field r a b
-- |A Lie Ring, composed of a commutative group and an anticommutative second operation. Distributivity and the Jacobi identity hold.
class (CommutativeGroup a, Anticommutative b, Distributive r, JacobiIdentity r,
       Eliminating r) => LieRing r a b

-- |A general ringlike structure which underlies
--  all the special cases (Rings, Fields, etc.)
data RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 =
   RinglikeStruct{rLeftDistributive::dl,
                  rRightDistributive::dr,
                  rAnnihiliate::an,
                  rJacobiIdentity::ja,
                  rEliminating::eli,
                  rZeroProduct::zp,
                  rPositiveProduct::pp,
                  rUniquelyFactorizable::un (el -> [(el, Int)]),
                  rEuclidean::euc,
                  rAbsorbing::ab,
                  rComplement::cp (Un el),
                  rStruct1::g1 el t1,
                  rStruct2::g2 el t2}

-- Show-instance for RinglikeStruct

instance (LeftDistributivityTag dl,
          RightDistributivityTag dr,
          AnnihilationTag an,
          JacobiIdentityTag ja,
          EliminatingTag eli,
          ZeroProductTag zp,
          PositiveProductTag pp,
          UniquelyFactorizableTag un,
          EuclideanTag euc,
          AbsorbingTag ab,
          ComplementTag cp,
          Show el,
          Show (g1 el t1),
          Show (g2 el t2),
          Show t1,
          Show t2) => Show (RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2) where
   show struct = printf "Ringlike (g1: %, g2: %) (%)" [show g1, show g2, implode ", " traits]
      where g1 = rStruct1 struct
            g2 = rStruct2 struct
            dl = getLeftDistributivityValue $ rLeftDistributive struct
            dr = getRightDistributivityValue $ rRightDistributive struct
            an = getAnnihilationValue $ rAnnihiliate struct
            ja = getJacobiIdentityValue $ rJacobiIdentity struct
            zp = getZeroProductValue $ rZeroProduct struct
            pp = getPositiveProductValue $ rPositiveProduct struct
            un = getUniquelyFactorizableValue $ rUniquelyFactorizable struct
            eu = getEuclideanValue $ rEuclidean struct
            ab = getAbsorbingValue $ rAbsorbing struct
            cp = getComplementValue $ rComplement struct
            traits = filter (not . null) $ [show dl, show dr, show an,
                                            show ja, show zp, show pp,
                                            show un, show eu, show ab,
                                            show cp]


-- Instances for primitive traits

instance Ringlike (RinglikeStruct dl dr an ja eli zp pp un euc ab cp) where
   getStruct1 = rStruct1
   getStruct2 = rStruct2

instance LeftDistributive (RinglikeStruct TagLeftDistributive dr an ja eli zp pp un euc ab cp)
instance RightDistributive (RinglikeStruct dl TagRightDistributive an ja eli zp pp un euc ab cp)
instance Distributive (RinglikeStruct TagLeftDistributive TagRightDistributive an ja eli zp pp un euc ab cp)
instance Annihilating (RinglikeStruct dl dr TagAnnihilating ja eli zp pp un euc ab cp)
instance JacobiIdentity (RinglikeStruct dl dr an TagJacobiIdentity eli zp pp un euc ab cp)
instance Eliminating (RinglikeStruct dl dr an ja TagEliminating zp pp un euc ab cp)
instance ZeroProduct (RinglikeStruct dl dr an ja eli TagZeroProduct pp un euc ab cp)
instance PositiveProduct (RinglikeStruct dl dr an ja eli zp TagPositiveProduct un euc ab cp)
instance UniquelyFactorizable (RinglikeStruct dl dr an ja eli zp pp TagUniquelyFactorizable euc ab cp) where
   factorize = (\(TagUniquelyFactorizable a) -> a) . rUniquelyFactorizable
instance Euclidean (RinglikeStruct dl dr an ja eli zp pp un TagEuclidean ab cp)
instance Absorbing (RinglikeStruct dl dr an ja el zp pp un euc TagAbsorbing cp)
instance Complement (RinglikeStruct dl dr an ja el zp pp un euc ab TagComplement) where
   complement = (\(TagComplement a) -> a) . rComplement

-- Instances for algebraic structures

instance (Semilattice a, Semilattice b) => Lattice (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (BoundedSemilattice a, BoundedSemilattice b) => BoundedLattice (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (Semilattice a, Semilattice b, CommutativeGroup a, Monoid b) => BooleanRing (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc TagAbsorbing TagComplement) a b
instance (CommutativeMonoid a, Monoid b) => Semiring (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (Group a, Semigroup b) => RightNearRing (RinglikeStruct dl TagRightDistributive an ja eli zp pp un euc ab cp) a b
instance (Group a, Semigroup b) => LeftNearRing (RinglikeStruct TagLeftDistributive dr an ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, Monoid b) => Ring (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, Monoid b) => Domain (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli TagZeroProduct pp un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => CommutativeRing (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => IntegralDomain (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => UniqueFactorizationDomain (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => EuclideanDomain (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable TagEuclidean ab cp) a b
instance (CommutativeGroup a, CommutativeGroup b) => Field (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable TagEuclidean ab cp) a b
instance (CommutativeGroup a, Anticommutative b) => LieRing (RinglikeStruct TagLeftDistributive TagRightDistributive an TagJacobiIdentity TagEliminating zp pp un euc ab cp) a b

-- |Type synonym for a dynamic ringlike structure which doesn't have static checks on its properties.
type DynamicRingStruct g1 g2 el t1 t2 = RinglikeStruct LeftDistributivityValue RightDistributivityValue AnnihilationValue JacobiIdentityValue EliminatingValue ZeroProductValue PositiveProductValue UniquelyFactorizableValue EuclideanValue AbsorbingValue ComplementValue g1 g2 el t1 t2

-- |Type synonym for a basic ringlike structure.
$(makeStructureTypeSynonym "BasicRinglikeStruct" "RinglikeStruct" [] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a lattice.
$(makeStructureTypeSynonym "LatticeStruct" "RinglikeStruct" ["LeftDistributive",
                                                             "RightDistributive",
                                                             "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a bounded lattice.
$(makeStructureTypeSynonym "BoundedLatticeStruct" "RinglikeStruct" ["LeftDistributive",
                                                                    "RightDistributive",
                                                                    "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Boolean ring.
$(makeStructureTypeSynonym "BooleanRingStruct" "RinglikeStruct" ["LeftDistributive",
                                                                 "RightDistributive",
                                                                 "Annihilating",
                                                                 "Absorbing",
                                                                 "Complement"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a semiring.
$(makeStructureTypeSynonym "SemiringStruct" "RinglikeStruct" ["LeftDistributive",
                                                              "RightDistributive",
                                                              "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a right near ring.
$(makeStructureTypeSynonym "RightNearRingStruct" "RinglikeStruct" ["RightDistributive"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a left near ring.
$(makeStructureTypeSynonym "LeftNearRingStruct" "RinglikeStruct" ["LeftDistributive"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a ring.
$(makeStructureTypeSynonym "RingStruct" "RinglikeStruct" ["LeftDistributive",
                                                          "RightDistributive",
                                                          "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a domain.
$(makeStructureTypeSynonym "DomainStruct" "RinglikeStruct" ["LeftDistributive",
                                                            "RightDistributive",
                                                            "Annihilating",
                                                            "ZeroProduct"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a commutative ring.
$(makeStructureTypeSynonym "CommutativeRingStruct" "RinglikeStruct" ["LeftDistributive",
                                                                     "RightDistributive",
                                                                     "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for an integral domain.
$(makeStructureTypeSynonym "IntegralDomainStruct" "RinglikeStruct" ["LeftDistributive",
                                                                    "RightDistributive",
                                                                    "Annihilating",
                                                                    "ZeroProduct",
                                                                    "PositiveProduct"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a unique factorization domain.
$(makeStructureTypeSynonym "UniqueFactorizationStruct" "RinglikeStruct" ["LeftDistributive",
                                                                         "RightDistributive",
                                                                         "Annihilating",
                                                                         "ZeroProduct",
                                                                         "PositiveProduct",
                                                                         "UniquelyFactorizable"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Euclidean domain.
$(makeStructureTypeSynonym "EuclideanDomainStruct" "RinglikeStruct" ["LeftDistributive",
                                                                     "RightDistributive",
                                                                     "Annihilating",
                                                                     "ZeroProduct",
                                                                     "PositiveProduct",
                                                                     "UniquelyFactorizable",
                                                                     "Euclidean"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a field.
$(makeStructureTypeSynonym "FieldStruct" "RinglikeStruct" ["LeftDistributive",
                                                           "RightDistributive",
                                                           "Annihilating",
                                                           "ZeroProduct",
                                                           "PositiveProduct",
                                                           "UniquelyFactorizable",
                                                           "Euclidean"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Lie ring.
$(makeStructureTypeSynonym "LieRingStruct" "RinglikeStruct" ["LeftDistributive",
                                                           "RightDistributive",
                                                           "JacobiIdentity",
                                                           "Eliminating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])


-- Addition of individual traits to a structure

-- |Adds left-distributivity to a structure.
addLeftDistributivity (RinglikeStruct _ dr an ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct TagLeftDistributive dr an ja eli zp pp un euc ab cp g1 g2

-- |Adds right-distributivity to a structure.
addRightDistributivity (RinglikeStruct dl _ an ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct dl TagRightDistributive an ja eli zp pp un euc ab cp g1 g2

-- |Adds left- and right-distributivity to a structure.
addDistributivity = addRightDistributivity . addLeftDistributivity

-- |Adds the annihilating property to a structure.
addAnnihilating (RinglikeStruct dl dr _ ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct dl dr TagAnnihilating ja eli zp pp un euc ab cp g1 g2

-- |Adds the property of the Jacobi identity to a structure.
addJacobiIdentity (RinglikeStruct dl dr an _ eli zp pp un euc ab cp g1 g2) = RinglikeStruct dl dr an TagJacobiIdentity eli zp pp un euc ab cp g1 g2

-- |Adds the eliminating property to a structure.
addEliminating (RinglikeStruct dl dr an ja _ zp pp un euc ab cp g1 g2) = RinglikeStruct dl dr an ja TagEliminating zp pp un euc ab cp g1 g2

-- |Adds the zero product property to a structure.
addZeroProduct (RinglikeStruct dl dr an ja eli _ pp un euc ab cp g1 g2) = RinglikeStruct dl dr an ja eli TagZeroProduct pp un euc ab cp g1 g2

-- |Adds the positive product property to a structure.
addPositiveProduct (RinglikeStruct dl dr an ja eli zp _ un euc ab cp g1 g2) = RinglikeStruct dl dr an ja eli zp TagPositiveProduct un euc ab cp g1 g2

-- |Adds unique factorization to a structure: every element must be able to be uniquely decomposed into its prime products.
addUniquelyFactorizable factor (RinglikeStruct dl dr an ja eli zp pp _ euc ab cp g1 g2) = RinglikeStruct dl dr an ja eli zp pp (TagUniquelyFactorizable factor) euc ab cp g1 g2

-- |Adds the Euclidean property to a structure: the greatest common divisor of two non-zero elements can be computed by a Euclidean function.
addEuclidean (RinglikeStruct dl dr an ja eli zp pp un _ ab cp g1 g2) = RinglikeStruct dl dr an ja eli zp pp un TagEuclidean ab cp g1 g2

-- |Adds absorbtion to a structure.
addAbsorbing (RinglikeStruct dl dr an ja eli zp pp un euc _ cp g1 g2) = RinglikeStruct dl dr an ja eli zp pp un euc TagAbsorbing cp g1 g2

-- |Adds the complement function to a structure.
addComplement comp (RinglikeStruct dl dr an ja eli zp pp un euc ab _ g1 g2) = RinglikeStruct dl dr an ja eli zp pp un euc ab (TagComplement comp) g1 g2

-- Creation of structures

-- |Creates a basic ringlike structure with two operations.
--  Nothing else is known about it.
makeRinglike :: (Grouplike g1, Grouplike g2)
             => g1 el t1 -- ^The first substructure.
             -> g2 el t2 -- ^The second substructure.
             -> BasicRinglikeStruct g1 g2 el t1 t2
makeRinglike = RinglikeStruct TagUnknownLeftDistributive TagUnknownRightDistributive TagUnknownAnnihilating TagUnknownJacobiIdentity TagUnknownEliminating TagUnknownZeroProduct TagUnknownPositiveProduct TagUnknownUniquelyFactorizable TagUnknownEuclidean TagUnknownAbsorbing TagUnknownComplement

makeLattice :: (Semilattice g1, Semilattice g2)
            => g1 el t1
            -> g2 el t2
            -> LatticeStruct g1 g2 el t1 t2
makeLattice g1 g2 = addAnnihilating $ addDistributivity $ makeRinglike g1 g2

makeBoundedLattice :: (BoundedSemilattice g1, BoundedSemilattice g2)
                   => g1 el t1
                   -> g2 el t2
                   -> BoundedLatticeStruct g1 g2 el t1 t2
makeBoundedLattice = makeLattice

--todo
makeBooleanRing = undefined

makeSemiring :: (CommutativeMonoid g1, Monoid g2)
            => g1 el t1
            -> g2 el t2
            -> SemiringStruct g1 g2 el t1 t2
makeSemiring g1 g2 = addAnnihilating $ addDistributivity $ makeRinglike g1 g2


{-makeRightNearRing :: ()
makeLeftNearRing
makeRing
makeDomain
makeCommutativeRing
makeIntegralDomain
makeUniqueFactorizationDomain
makeEuclideanDomain
makeField
makeLieRing-}


-- |Turns a statically typechecked ringllike structure into a one without it.
--  Whereas the values the type parameters can take in the input will be unrelated,
--  the type parameters int the output will belong to sum types.
--  The resulting structure will thereby be able to bypass all static type checks and
--  be accepted anywhere where a (dynamic) ringlike structure is accepted.
makeDynamic r = (RinglikeStruct (isLeftDistributive r)
                                (isRightDistributive r)
                                (isAnnihilating r)
                                (hasJacobiIdentity r)
                                (isEliminating r)
                                (hasZeroProduct r)
                                (hasPositiveProduct r)
                                (getFactorizationFunction r)
                                (isEuclidean r)
                                (isAbsorbing r)
                                (hasComplement r)
                                (G.makeDynamic $ rStruct1 r)
                                (G.makeDynamic $ rStruct2 r))

-- Typesafe accessor functions for structures

-- |Returns whether the structure is left-distributive.
isLeftDistributive :: (LeftDistributivityTag dl) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> LeftDistributivityValue
isLeftDistributive = getLeftDistributivityValue . rLeftDistributive

-- |Returns whether the structure is right-distributive.
isRightDistributive :: (RightDistributivityTag dr) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> RightDistributivityValue
isRightDistributive = getRightDistributivityValue . rRightDistributive

-- |Returns whether the structure is annihilating.
isAnnihilating :: (AnnihilationTag an) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> AnnihilationValue
isAnnihilating = getAnnihilationValue . rAnnihiliate

-- |Returns whether the structure obeys the Jacobi identity.
hasJacobiIdentity :: (JacobiIdentityTag ja) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> JacobiIdentityValue
hasJacobiIdentity = getJacobiIdentityValue . rJacobiIdentity

-- |Returns whether the structure is eliminating.
isEliminating :: (EliminatingTag eli) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> EliminatingValue
isEliminating = getEliminatingValue . rEliminating

-- |Returns whether a zero product implies zero factors in a structure.
hasZeroProduct :: (ZeroProductTag zp) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> ZeroProductValue
hasZeroProduct = getZeroProductValue . rZeroProduct

-- |Returns whether the product of two non-zero elements is non-zero in the structure.
hasPositiveProduct :: (PositiveProductTag pp) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> PositiveProductValue
hasPositiveProduct = getPositiveProductValue . rPositiveProduct

-- |Returns whether the structure is unique factorizable
getFactorizationFunction :: (UniquelyFactorizableTag un) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> UniquelyFactorizableValue (el -> [(el, Int)])
getFactorizationFunction = getUniquelyFactorizableValue . rUniquelyFactorizable

-- |Returns whether the structure is Euclidean.
isEuclidean :: (EuclideanTag euc) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> EuclideanValue
isEuclidean = getEuclideanValue . rEuclidean

-- |Returns whether the structure is absorbing.
isAbsorbing :: (AbsorbingTag ab) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> AbsorbingValue
isAbsorbing = getAbsorbingValue . rAbsorbing

-- |Returns whether the structure has a complement operation.
hasComplement :: (ComplementTag cp) => RinglikeStruct dl dr an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> ComplementValue (el -> el)
hasComplement = getComplementValue . rComplement