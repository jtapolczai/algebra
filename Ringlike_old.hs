{-# LANGUAGE FlexibleInstances #-}

module Ringlike
  (UO,
   BO,
   SemiringStruct,
   StarsemiringStruct,
   RingStruct,
   CommutativeRingStruct,
   DomainStruct,
   IntegralDomainStruct,
   IntegrallyClosedDomainStruct,
   UniqueFactorizationDomainStruct,
   PrincipalIdealDomainStruct,
   EuclideanDomainStruct,
   FieldStruct,
   makeSemiring,
   makeStarsemiring,
   makeRing,
   makeCommutativeRing,
   makeDomain,
   makeIntegralDomain,
   makeIntegrallyClosedDomain,
   makeUniqueFactorizationDomain,
   makePrincipalIdealDomain,
   makeField)
where

import Grouplike

data LatticeTag = LatticeTag
data BoundedLatticeTag = BoundedLatticeTag
data SemiringTag = SemiringTag
data StarsemiringTag = StarsemiringTag
data RingTag = RingTag
data CommutativeRingTag = CommutativeRingTag
data DomainTag = DomainTag
data IntegralDomainTag = IntegralDomainTag
data IntegrallyClosedDomainTag = IntegrallyClosedDomainTag
data UniqueFactorizationDomainTag = UniqueFactorizationDomainTag
data PrincipalIdealDomainTag = PrincipalIdealDomainTag
data EuclideanDomainTag = EuclideanDomainTag
data FieldTag = FieldTag

type LatticeStruct el kind = RinglikeStruct LatticeTag el kind
type BoundedLatticeStruct el kind = RinglikeStruct BoundedLatticeTag el kind
type SemiringStruct el kind = RinglikeStruct SemiringTag el kind
type StarsemiringStruct el kind = RinglikeStruct StarsemiringTag el kind
type RingStruct el kind = RinglikeStruct RingTag el kind
type CommutativeRingStruct el kind = RinglikeStruct CommutativeRingTag el kind
type DomainStruct el kind = RinglikeStruct DomainTag el kind
type IntegralDomainStruct el kind = RinglikeStruct IntegralDomainTag el kind
type IntegrallyClosedDomainStruct el kind = RinglikeStruct IntegrallyClosedDomainTag el kind
type UniqueFactorizationDomainStruct el kind = RinglikeStruct UniqueFactorizationDomainTag el kind
type PrincipalIdealDomainStruct el kind = RinglikeStruct PrincipalIdealDomainTag el kind
type EuclideanDomainStruct el kind = RinglikeStruct EuclideanDomainTag el kind
type FieldStruct el kind = RinglikeStruct FieldTag el kind

class RinglikeTag a where

instance RinglikeTag LatticeTag where
instance RinglikeTag BoundedLatticeTag where
instance RinglikeTag SemiringTag where
instance RinglikeTag StarsemiringTag where
instance RinglikeTag RingTag where
instance RinglikeTag CommutativeRingTag where
instance RinglikeTag DomainTag where
instance RinglikeTag IntegralDomainTag where
instance RinglikeTag IntegrallyClosedDomainTag where
instance RinglikeTag UniqueFactorizationDomainTag where
instance RinglikeTag EuclideanDomainTag where
instance RinglikeTag FieldTag where

data RinglikeStruct tag el kind1 kind2 =
  RinglikeStruct{rTag::tag,
                 rkind1::kind1,
                 rkind2::kind2,
                 rldiv1::BO el,
                 rrdiv1::BO el,
                 rldiv2::BO el,
                 rrdiv2::BO el,
                 rinv1::BO el,
                 rinv2::BO el,
                 rstar::UO el,
                 rop1::BO el,
                 rop2::BO el,
                 rident1::el,
                 rident2::el}

class Lattice a where
  lop1 :: a el k1 k2 -> BO el
  lop2 :: a el k1 k2 -> BO el
  getJoinSemilattice :: (Semilattice m) => a el k1 k2 -> m el k1
  getMeetSemilattice :: (Semilattice m) => a el k1 k2 -> m el k2

class Lattice a => BoundedLattice a where
  lident1 :: a el k1 k2 -> el
  lident2 :: a el k1 k2 -> el
  getJoinBoundedSemilattice :: (BoundedSemilattice m) => a el k1 k2 -> m el k1
  getMeetBoundedSemilattice :: (BoundedSemilattice m) => a el k1 k2 -> m el k2

class Semiring a where
  op1 :: a el k1 k2 -> BO el
  op2 :: a el k1 k2 -> BO el
  ident1 :: a el k1 k2 -> el
  ident2 :: a el k1 k2 -> el
  getAdditiveCommutativeMonoid :: (CommutativeMonoid m) => a el k1 k2 -> m el k1
  getMultiplicativeMonoid :: (Monoid m) => a el k1 k2 -> m el k2

class Semiring a => Starsemiring a where
  star :: a el k1 k2 -> UO el

class Semiring a => Ring a where
  inv1 :: a el k1 k2 -> UO el
  getAdditiveCommutativeGroup :: (CommutativeGroup m) => a el k1 k2 -> m el k1

class Ring a => CommutativeRing a where
  getMultiplicativeCommutativeMonoid :: (CommutativeMonoid m) => a el k1 k2 -> m el k1
