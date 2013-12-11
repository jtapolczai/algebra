{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
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

data RinglikeStruct dl dr an g1 g2 t =
   RinglikeStruct{rLeftDistributive::dl,
                  rRightDistributive::dr,
                  rAnnihiliate::an,
                  rStruct1::g1,
                  rStruct2::g2,
                  rTag::t}

class Ringlike r where
   getStruct1 :: Group g1 => r (g1 el t1) (g2 el t2) t -> (g1 el t1)
   getStruct2 :: Monoid g2 => r (g1 el t1) (g2 el t2) t -> (g2 el t2)
class Semiring r where
--   getGroup :: r el t1 t2 -> GroupStruct el t1
class Semiring r => Ring r where
class Ring r => CommutativeRing r where

instance Ringlike (RinglikeStruct dl dr an) where
   getStruct1 = rStruct1
   getStruct2 = rStruct2

m1 = makeGroup (+) (*(-1)) 0 "add"
m2 = makeMonoid (*) 0 "mul"

--instance Semiring (RinglikeStruct dl dr an GroupStruct g2) where
--   getGroup = rStruct1 

--instance (Group a, Monoid b) => Semiring (r a b) where
--instance (CommutativeGroup a, Monoid b) => Ring (r a b) where
--instance (Ring (r a b), Commutative a) => CommutativeRing (r a b) where
