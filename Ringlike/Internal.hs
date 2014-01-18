{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}

{-|
  Grants access to the internal parts of ringlike structures, allowing
  the circumvention of the static type checks and the querying structural
  properties at runtime.

  Normally, checking structures for desired properties
  is a purely static action done by the type checker through contexts,
  but here, we can access the structures directly. Heterogenous lists
  of structures are also possible after turning them into dynamic ones via
  @makeDynamic@.

  For more information, see @Grouplike.Internal@.
-}
module Ringlike.Internal (
  module Ringlike.Traits,

  -- * Common ringlike algebraic structure
  RinglikeStruct,

  -- * Turning static structures into dynamic ones
  makeDynamic,

  -- * Predfined classes
  Lattice,
  BoundedLattice,
  BooleanLattice,
  Semiring,
  RightNearRing,
  LeftNearRing,
  Rng,
  Ring,
  Domain,
  CommutativeRing,
  IntegralDomain,
  UniqueFactorizationDomain,
  EuclideanDomain,
  Field,
  LieRing,

  -- * Shorthand type synonyms for common structures
  DynamicRingStruct,
  BasicRinglikeStruct,
  LatticeStruct,
  BoundedLatticeStruct,
  BooleanLatticeStruct,
  SemiringStruct,
  RightNearRingStruct,
  LeftNearRingStruct,
  RngStruct,
  RingStruct,
  DomainStruct,
  CommutativeRingStruct,
  IntegralDomainStruct,
  UniqueFactorizationDomainStruct,
  EuclideanDomainStruct,
  FieldStruct,
  LieRingStruct,

  -- * Constructors
  makeLattice,
  makeBoundedLattice,
  makeBooleanLattice,
  makeSemiring,
  makeRightNearRing,
  makeLeftNearRing,
  makeRng,
  makeRing,
  makeDomain,
  makeCommutativeRing,
  makeIntegralDomain,
  makeUniqueFactorizationDomain,
  makeEuclideanDomain,
  makeField,
  makeLieRing,

  -- * Adding traits to structures
  addLeftDistributivity,
  addRightDistributivity,
  addDistributivity,
  addLeftAnnihilating,
  addRightAnnihilating,
  addAnnihilating,
  addJacobiIdentity,
  addEliminating,
  addZeroProduct,
  addPositiveProduct,
  addUniquelyFactorizable,
  addEuclidean,
  addAbsorbing,
  addComplement,

  -- * Checking for the presence of traits
  isDistributive,
  isAnnihilating,
  hasJacobiIdentity,
  isEliminating,
  hasZeroProduct,
  hasPositiveProduct,
  getFactorizationFunction,
  isEuclidean,
  isAbsorbing,
  hasComplement
  ) where

import Control.Applicative
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
class (Lattice r a b, Ring r a b) => BooleanLattice r a b
-- |A semiring, composed of a commutative additive monoid and a multiplicative monoid. 
class (CommutativeMonoid a, Monoid b, Distributive r, Annihilating r) => Semiring r a b
-- |A right near-ring where the second operation does not have a unit element and only right-distributivity holds.
class (Group a, Semigroup b, RightDistributive r) => RightNearRing r a b
-- |A left near-ring where the second operation does not have a unit element and only left-distributivity holds.
class (Group a, Semigroup b, LeftDistributive r) => LeftNearRing r a b
-- |A ring without a multiplicative identity. The second component is a semigroup.
class (CommutativeGroup a, Semigroup b, Distributive r, Annihilating r) => Rng r a b
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
data RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 =
   RinglikeStruct{rDistributive::d,
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

instance (DistributivityTag d,
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
          Show t2) => Show (RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2) where
   show struct = printf "Ringlike (g1: %, g2: %) (%)" [show g1, show g2, implode ", " traits]
      where g1 = rStruct1 struct
            g2 = rStruct2 struct
            di = getDistributivityValue $ rDistributive struct
            an = getAnnihilationValue $ rAnnihiliate struct
            ja = getJacobiIdentityValue $ rJacobiIdentity struct
            zp = getZeroProductValue $ rZeroProduct struct
            pp = getPositiveProductValue $ rPositiveProduct struct
            un = getUniquelyFactorizableValue $ rUniquelyFactorizable struct
            eu = getEuclideanValue $ rEuclidean struct
            ab = getAbsorbingValue $ rAbsorbing struct
            cp = getComplementValue $ rComplement struct
            traits = filter (not . null) $ [show di, show an, show ja,
                                            show zp, show pp, show un,
                                            show eu, show ab, show cp]


-- Instances for primitive traits

instance Ringlike (RinglikeStruct d an ja eli zp pp un euc ab cp) where
   getStruct1 = rStruct1
   getStruct2 = rStruct2

instance LeftDistributive (RinglikeStruct TagLeftDistributive an ja eli zp pp un euc ab cp)
instance RightDistributive (RinglikeStruct TagRightDistributive an ja eli zp pp un euc ab cp)
instance LeftDistributive (RinglikeStruct TagDistributive an ja eli zp pp un euc ab cp)
instance RightDistributive (RinglikeStruct TagDistributive an ja eli zp pp un euc ab cp)
instance Distributive (RinglikeStruct TagDistributive an ja eli zp pp un euc ab cp)

instance LeftAnnihilating (RinglikeStruct d TagLeftAnnihilating ja eli zp pp un euc ab cp)
instance RightAnnihilating (RinglikeStruct d TagRightAnnihilating ja eli zp pp un euc ab cp)
instance LeftAnnihilating (RinglikeStruct d TagAnnihilating ja eli zp pp un euc ab cp)
instance RightAnnihilating (RinglikeStruct d TagAnnihilating ja eli zp pp un euc ab cp)
instance Annihilating (RinglikeStruct d TagAnnihilating ja eli zp pp un euc ab cp)

instance JacobiIdentity (RinglikeStruct d an TagJacobiIdentity eli zp pp un euc ab cp)
instance Eliminating (RinglikeStruct d an ja TagEliminating zp pp un euc ab cp)
instance ZeroProduct (RinglikeStruct d an ja eli TagZeroProduct pp un euc ab cp)
instance PositiveProduct (RinglikeStruct d an ja eli zp TagPositiveProduct un euc ab cp)
instance UniquelyFactorizable (RinglikeStruct d an ja eli zp pp TagUniquelyFactorizable euc ab cp) where
   factorize = (\(TagUniquelyFactorizable a) -> a) . rUniquelyFactorizable
instance Euclidean (RinglikeStruct d an ja eli zp pp un TagEuclidean ab cp)
instance Absorbing (RinglikeStruct d an ja el zp pp un euc TagAbsorbing cp)
instance Complement (RinglikeStruct d an ja el zp pp un euc ab TagComplement) where
   complement = (\(TagComplement a) -> a) . rComplement

-- Instances for algebraic structures
--rewrite this


instance (Semilattice a, Semilattice b) => Lattice (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (BoundedSemilattice a, BoundedSemilattice b) => BoundedLattice (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (Semilattice a, Semilattice b, CommutativeGroup a, Monoid b) => BooleanLattice (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc TagAbsorbing TagComplement) a b
instance (CommutativeMonoid a, Monoid b) => Semiring (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b

instance (Group a, Semigroup b) => RightNearRing (RinglikeStruct TagRightDistributive TagRightAnnihilating ja eli zp pp un euc ab cp) a b
instance (Group a, Semigroup b) => LeftNearRing (RinglikeStruct TagLeftDistributive TagLeftAnnihilating ja eli zp pp un euc ab cp) a b

instance (CommutativeGroup a, Semigroup b) => Rng (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, Monoid b) => Ring (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, Monoid b) => Domain (RinglikeStruct TagDistributive TagAnnihilating ja eli TagZeroProduct pp un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => CommutativeRing (RinglikeStruct TagDistributive TagAnnihilating ja eli zp pp un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => IntegralDomain (RinglikeStruct TagDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct un euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => UniqueFactorizationDomain (RinglikeStruct TagDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable euc ab cp) a b
instance (CommutativeGroup a, CommutativeMonoid b) => EuclideanDomain (RinglikeStruct TagDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable TagEuclidean ab cp) a b
instance (CommutativeGroup a, CommutativeGroup b) => Field (RinglikeStruct TagDistributive TagAnnihilating ja eli TagZeroProduct TagPositiveProduct TagUniquelyFactorizable TagEuclidean ab cp) a b
instance (CommutativeGroup a, Anticommutative b) => LieRing (RinglikeStruct TagDistributive an TagJacobiIdentity TagEliminating zp pp un euc ab cp) a b

-- |Type synonym for a dynamic ringlike structure which doesn't have static checks on its properties.
type DynamicRingStruct g1 g2 el t1 t2 = RinglikeStruct DistributivityValue AnnihilationValue JacobiIdentityValue EliminatingValue ZeroProductValue PositiveProductValue UniquelyFactorizableValue EuclideanValue AbsorbingValue ComplementValue g1 g2 el t1 t2

-- |Type synonym for a basic ringlike structure.
$(makeStructureTypeSynonym "BasicRinglikeStruct" "RinglikeStruct" [] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a lattice.
$(makeStructureTypeSynonym' "LatticeStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a bounded lattice.
$(makeStructureTypeSynonym' "BoundedLatticeStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Boolean ring.
$(makeStructureTypeSynonym' "BooleanLatticeStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "Absorbing",
   "Complement"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a semiring.
$(makeStructureTypeSynonym' "SemiringStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a right near ring.
$(makeStructureTypeSynonym "RightNearRingStruct" "RinglikeStruct"
  [("Distributive", "RightDistributive"),
   ("Annihilating", "RightAnnihilating")] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a left near ring.
$(makeStructureTypeSynonym "LeftNearRingStruct" "RinglikeStruct"
  [("Distributive", "LeftDistributive"),
   ("Annihilating", "LeftAnnihilating")] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a ring.
$(makeStructureTypeSynonym' "RngStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a ring.
$(makeStructureTypeSynonym' "RingStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a domain.
$(makeStructureTypeSynonym' "DomainStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "ZeroProduct"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a commutative ring.
$(makeStructureTypeSynonym' "CommutativeRingStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for an integral domain.
$(makeStructureTypeSynonym' "IntegralDomainStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "ZeroProduct",
   "PositiveProduct"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a unique factorization domain.
$(makeStructureTypeSynonym' "UniqueFactorizationDomainStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "ZeroProduct",
   "PositiveProduct",
   "UniquelyFactorizable"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Euclidean domain.
$(makeStructureTypeSynonym' "EuclideanDomainStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "ZeroProduct",
   "PositiveProduct",
   "UniquelyFactorizable",
   "Euclidean"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a field.
$(makeStructureTypeSynonym' "FieldStruct" "RinglikeStruct"
  ["Distributive",
   "Annihilating",
   "ZeroProduct",
   "PositiveProduct",
   "UniquelyFactorizable",
   "Euclidean"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])
-- |Type synonym for a Lie ring.
$(makeStructureTypeSynonym' "LieRingStruct" "RinglikeStruct"
  ["Distributive",
   "JacobiIdentity",
   "Eliminating"] ringlikeTraits ["g1", "g2", "el", "t1", "t2"])


-- Addition of individual traits to a structure

-- |Sets the structure to be left-distributive.
addLeftDistributivity (RinglikeStruct _ an ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct TagLeftDistributive an ja eli zp pp un euc ab cp g1 g2

-- |Sets the structure to be right-distributive.
addRightDistributivity (RinglikeStruct _ an ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct TagRightDistributive an ja eli zp pp un euc ab cp g1 g2

-- |Sets the structure to be distributive
addDistributivity (RinglikeStruct _ an ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct TagDistributive an ja eli zp pp un euc ab cp g1 g2

-- |Sets the structure to be left-annihilating.
addLeftAnnihilating (RinglikeStruct d _ ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct d TagLeftAnnihilating ja eli zp pp un euc ab cp g1 g2

-- |Sets the structure to be right-annihilating.
addRightAnnihilating (RinglikeStruct d _ ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct d TagRightAnnihilating ja eli zp pp un euc ab cp g1 g2

-- |Sets the structure to be annihilating.
addAnnihilating (RinglikeStruct d _ ja eli zp pp un euc ab cp g1 g2) = RinglikeStruct d TagAnnihilating ja eli zp pp un euc ab cp g1 g2

-- |Adds the property of the Jacobi identity to a structure.
addJacobiIdentity (RinglikeStruct d an _ eli zp pp un euc ab cp g1 g2) = RinglikeStruct d an TagJacobiIdentity eli zp pp un euc ab cp g1 g2

-- |Adds the eliminating property to a structure.
addEliminating (RinglikeStruct d an ja _ zp pp un euc ab cp g1 g2) = RinglikeStruct d an ja TagEliminating zp pp un euc ab cp g1 g2

-- |Adds the zero product property to a structure.
addZeroProduct (RinglikeStruct d an ja eli _ pp un euc ab cp g1 g2) = RinglikeStruct d an ja eli TagZeroProduct pp un euc ab cp g1 g2

-- |Adds the positive product property to a structure.
addPositiveProduct (RinglikeStruct d an ja eli zp _ un euc ab cp g1 g2) = RinglikeStruct d an ja eli zp TagPositiveProduct un euc ab cp g1 g2

-- |Adds unique factorization to a structure: every element must be able to be uniquely decomposed into its prime products.
addUniquelyFactorizable factor (RinglikeStruct d an ja eli zp pp _ euc ab cp g1 g2) = RinglikeStruct d an ja eli zp pp (TagUniquelyFactorizable factor) euc ab cp g1 g2

-- |Adds the Euclidean property to a structure: the greatest common divisor of two non-zero elements can be computed by a Euclidean function.
addEuclidean (RinglikeStruct d an ja eli zp pp un _ ab cp g1 g2) = RinglikeStruct d an ja eli zp pp un TagEuclidean ab cp g1 g2

-- |Adds absorbtion to a structure.
addAbsorbing (RinglikeStruct d an ja eli zp pp un euc _ cp g1 g2) = RinglikeStruct d an ja eli zp pp un euc TagAbsorbing cp g1 g2

-- |Adds the complement function to a structure.
addComplement cp (RinglikeStruct d an ja eli zp pp un euc ab _ g1 g2) = RinglikeStruct d an ja eli zp pp un euc ab (TagComplement cp) g1 g2

-- Creation of structures

-- |Creates a basic ringlike structure with two operations.
--  Nothing else is known about it.
makeRinglike :: (Grouplike g1, Grouplike g2)
             => g1 el t1 -- ^The structure's first component.
             -> g2 el t2 -- ^The structure's second component.
             -> BasicRinglikeStruct g1 g2 el t1 t2
makeRinglike = RinglikeStruct TagUnknownDistributive TagUnknownAnnihilating TagUnknownJacobiIdentity TagUnknownEliminating TagUnknownZeroProduct TagUnknownPositiveProduct TagUnknownUniquelyFactorizable TagUnknownEuclidean TagUnknownAbsorbing TagUnknownComplement

-- |Creates a lattice.
makeLattice :: (Semilattice g1, Semilattice g2)
            => g1 el t1 -- ^The structure's first component.
            -> g2 el t2 -- ^The structure's second component.
            -> LatticeStruct g1 g2 el t1 t2
makeLattice g1 g2 = addAnnihilating $ addDistributivity $ makeRinglike g1 g2

-- |Creates a bounded lattice.
makeBoundedLattice :: (BoundedSemilattice g1, BoundedSemilattice g2)
                   => g1 el t1 -- ^The structure's first component.
                   -> g2 el t2 -- ^The structure's second component.
                   -> BoundedLatticeStruct g1 g2 el t1 t2
makeBoundedLattice = makeLattice

-- |Creates a Boolean lattice.
makeBooleanLattice :: (BoundedSemilattice g1, BoundedSemilattice g2)
                   => g1 el t1 -- ^The structure's first component.
                   -> g2 el t2 -- ^The structure's second component.
                   -> (Un el) -- ^The complement function.
                   -> BooleanLatticeStruct g1 g2 el t1 t2
makeBooleanLattice g1 g2 cp = addComplement cp $ addAbsorbing $ makeLattice g1 g2

-- |Creates a semiring.
makeSemiring :: (CommutativeMonoid g1, Monoid g2)
            => g1 el t1 -- ^The structure's first component.
            -> g2 el t2 -- ^The structure's second component.
            -> SemiringStruct g1 g2 el t1 t2
makeSemiring g1 g2 = addAnnihilating $ addDistributivity $ makeRinglike g1 g2

-- |Creates a rng.
makeRng :: (CommutativeGroup g1, Semigroup g2)
        => g1 el t1 -- ^The structure's first component.
        -> g2 el t2 -- ^The structure's second component.
        -> RngStruct g1 g2 el t1 t2
makeRng g1 g2 = addAnnihilating $ addDistributivity $ makeRinglike g1 g2

-- |Creates a right near ring.
makeRightNearRing :: (Group g1, Semigroup g2)
                  => g1 el t1 -- ^The structure's first component.
                  -> g2 el t2 -- ^The structure's second component.
                  -> RightNearRingStruct g1 g2 el t1 t2
makeRightNearRing g1 g2 = addRightAnnihilating $ addRightDistributivity $ makeRinglike g1 g2

-- |Creates a left near ring.
makeLeftNearRing :: (Group g1, Semigroup g2)
                 => g1 el t1 -- ^The structure's first component.
                 -> g2 el t2 -- ^The structure's second component.
                 -> LeftNearRingStruct g1 g2 el t1 t2
makeLeftNearRing g1 g2 = addLeftAnnihilating $ addLeftDistributivity $ makeRinglike g1 g2

-- |Creates a ring.
makeRing :: (CommutativeGroup g1, Monoid g2)
         => g1 el t1 -- ^The structure's first component.
         -> g2 el t2 -- ^The structure's second component.
         -> RingStruct g1 g2 el t1 t2
makeRing = makeSemiring

-- |Creates a domain.
makeDomain :: (CommutativeGroup g1, Monoid g2)
           => g1 el t1 -- ^The structure's first component.
           -> g2 el t2 -- ^The structure's second component.
           -> DomainStruct g1 g2 el t1 t2
makeDomain g1 g2 = addZeroProduct $ makeRing g1 g2

-- |Creates a commutative ring.
makeCommutativeRing :: (CommutativeGroup g1, CommutativeMonoid g2)
                    => g1 el t1 -- ^The structure's first component.
                    -> g2 el t2 -- ^The structure's second component.
                    -> CommutativeRingStruct g1 g2 el t1 t2
makeCommutativeRing = makeRing

-- |Creates an integral domain.
makeIntegralDomain :: (CommutativeGroup g1, CommutativeMonoid g2)
                   => g1 el t1 -- ^The structure's first component.
                   -> g2 el t2 -- ^The structure's second component.
                   -> IntegralDomainStruct g1 g2 el t1 t2
makeIntegralDomain g1 g2 = addPositiveProduct $ addZeroProduct $ makeCommutativeRing g1 g2

-- |Creates a unique factorization domain.
makeUniqueFactorizationDomain :: (CommutativeGroup g1, CommutativeMonoid g2)
                   => g1 el t1 -- ^The structure's first component.
                   -> g2 el t2 -- ^The structure's second component.
                   -> (el -> [(el,Int)]) -- ^The factorization function.
                   -> UniqueFactorizationDomainStruct g1 g2 el t1 t2
makeUniqueFactorizationDomain g1 g2 f = addUniquelyFactorizable f $ makeIntegralDomain g1 g2

-- |Creates a Euclidean domain.
makeEuclideanDomain :: (CommutativeGroup g1, CommutativeMonoid g2)
                    => g1 el t1 -- ^The structure's first component.
                    -> g2 el t2 -- ^The structure's second component.
                    -> (el -> [(el,Int)]) -- ^The factorization function.
                    -> EuclideanDomainStruct g1 g2 el t1 t2
makeEuclideanDomain g1 g2 f = addEuclidean $ makeUniqueFactorizationDomain g1 g2 f

-- |Creates a field.
makeField :: (CommutativeGroup g1, CommutativeGroup g2)
          => g1 el t1 -- ^The structure's first component.
          -> g2 el t2 -- ^The structure's second component.
          -> (el -> [(el,Int)]) -- ^The factorization function.
          -> FieldStruct g1 g2 el t1 t2
makeField = makeEuclideanDomain

-- |Creates a lie ring.
makeLieRing :: (CommutativeGroup g1, Anticommutative g2)
            => g1 el t1 -- ^The structure's first component.
            -> g2 el t2 -- ^The structure's second component.
            -> LieRingStruct g1 g2 el t1 t2
makeLieRing g1 g2 = addJacobiIdentity $ addEliminating $ addDistributivity $ makeRinglike g1 g2 


-- |Turns a statically typechecked ringllike structure into a one without it.
--  Whereas the values the type parameters can take in the input will be unrelated,
--  the type parameters int the output will belong to sum types.
--  The resulting structure will thereby be able to bypass all static type checks and
--  be accepted anywhere where a (dynamic) ringlike structure is accepted.
makeDynamic r = (RinglikeStruct (isDistributive r)
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

-- |Returns whether the structure is distributive.
isDistributive :: (DistributivityTag d) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> DistributivityValue
isDistributive = getDistributivityValue . rDistributive

-- |Returns whether the structure is annihilating.
isAnnihilating :: (AnnihilationTag an) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> AnnihilationValue
isAnnihilating = getAnnihilationValue . rAnnihiliate

-- |Returns whether the structure obeys the Jacobi identity.
hasJacobiIdentity :: (JacobiIdentityTag ja) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> JacobiIdentityValue
hasJacobiIdentity = getJacobiIdentityValue . rJacobiIdentity

-- |Returns whether the structure is eliminating.
isEliminating :: (EliminatingTag eli) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> EliminatingValue
isEliminating = getEliminatingValue . rEliminating

-- |Returns whether a zero product implies zero factors in a structure.
hasZeroProduct :: (ZeroProductTag zp) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> ZeroProductValue
hasZeroProduct = getZeroProductValue . rZeroProduct

-- |Returns whether the product of two non-zero elements is non-zero in the structure.
hasPositiveProduct :: (PositiveProductTag pp) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> PositiveProductValue
hasPositiveProduct = getPositiveProductValue . rPositiveProduct

-- |Returns whether the structure is unique factorizable
getFactorizationFunction :: (UniquelyFactorizableTag un) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> UniquelyFactorizableValue (el -> [(el, Int)])
getFactorizationFunction = getUniquelyFactorizableValue . rUniquelyFactorizable

-- |Returns whether the structure is Euclidean.
isEuclidean :: (EuclideanTag euc) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> EuclideanValue
isEuclidean = getEuclideanValue . rEuclidean

-- |Returns whether the structure is absorbing.
isAbsorbing :: (AbsorbingTag ab) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> AbsorbingValue
isAbsorbing = getAbsorbingValue . rAbsorbing

-- |Returns whether the structure has a complement operation.
hasComplement :: (ComplementTag cp) => RinglikeStruct d an ja eli zp pp un euc ab cp g1 g2 el t1 t2 -> ComplementValue (Un el)
hasComplement = getComplementValue . rComplement