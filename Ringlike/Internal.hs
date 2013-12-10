{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ringlike.Internal where

import Grouplike.Internal

-- |A general grouplike structure which underlies
--  all the special cases (Semigroups, Monoids, etc.)
{-data GrouplikeStruct c a i u l r inv el t = 
  GrouplikeStruct{gCommutativity::c,
                  gAssociativity::a,
                  gIdempotence::i,
                  gUnitElement::u el,
                  gLeftDivider::l (Bin el),
                  gRightDivider::r (Bin el),
                  gInverse::inv (Un el),
                  gOperation::Bin el,
                  -- |Gets the tag of the structure.
                  gTag::t}-}

--left-distributive
--right-distributive
--0 annihilates *
--absorption (into grouplikes?)

--mix non-algebraic traits (PO/TO?) into structures?
--Order(Structure)?

data RinglikeStruct dl dr an g1 g2 el t =
   RinglikeStruct{rLeftDistributive::dl,
                  rRightDistributive::dr,
                  rAnnihiliate::an,
                  rStruct1::g1,
                  rStruct2::g2,
                  rTag::t}

class Semiring r where
class Semiring r => Ring r where
class Ring r => CommutativeRing r where

instance (Group a, Monoid b) => Semiring (r a b) where
instance (CommutativeGroup a, Monoid b) => Ring (r a b) where
--instance (Ring (r a b), Commutative a) => CommutativeRing (r a b) where
