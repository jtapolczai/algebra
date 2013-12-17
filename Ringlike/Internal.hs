{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ringlike.Internal where

import Grouplike.Internal
import Ringlike.Traits

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

data RinglikeStruct dl dr an g1 g2 el t1 t2 =
   RinglikeStruct{rLeftDistributive::dl,
                  rRightDistributive::dr,
                  rAnnihiliate::an,
                  rStruct1::g1 el t1,
                  rStruct2::g2 el t2,
                  rTag::(t1,t2)}

class (CommutativeMonoid a, Monoid b) => Semiring r a b where
   getS1 :: r a b el t1 t2 -> (a el t1)

instance (CommutativeMonoid a, Monoid b) => Semiring (RinglikeStruct dl dr an) a b where
   getS1 = rStruct1

class (CommutativeGroup a, Monoid b) => Ring r a b where
   getSR1 :: r a b el t1 t2 -> (a el t1)

instance (CommutativeGroup a, Monoid b) => Ring (RinglikeStruct dl dr an) a b where
   getSR1 = rStruct1

f :: (Ring r a b, Commutative b) => r a b el t1 t2 -> [el] -> el
f r = foldl o i
   where o = op $ getSR1 r
         i = ident $ getSR1 r

r1 = RinglikeStruct 0 0 0 (makeCommutativeGroup (+) undefined 0 "add") (makeCommutativeMonoid (*) 1 "mult") ("add","mult")

{-class Semiring r where
   getCommutativeMonoid :: CommutativeMonoid g1 => r (g1 el t1) (g2 el t2) t -> (g1 el t1)
   getMonoid :: Monoid g2 => r (g1 el t1) (g2 el t2) t -> (g2 el t2)
class RightNearRing r where
   getGroup :: CommutativeGroup g1 => r (g1 el t1) (g2 el t2) t -> (g1 el t1)
class Semiring r => Ring r where
   getCommutativeGroup :: CommutativeGroup g1 => r (g1 el t1) (g2 el t2) t -> (g1 el t1)
--class Ring r => CommutativeRing r where
--   getCommutativeMonoid :: CommutativeMonoid g2 => r (g1 el t1) (g2 el t2) t -> (g2 el t2)

--todo: fields, integral domains, euclidean fields, lie rings,...


-- These definitions might look strange, since all the instances
-- just have "rStruct1/2" as the definitions, but the
-- class contexts on them in the classes ensure that one
-- cannot, say, pass off a semiring, which only has a group and a monoid
-- in it, as a ring, where the group also has to be commutative.

instance Ringlike (RinglikeStruct dl dr an) where
   getStruct1 = rStruct1
   getStruct2 = rStruct2

instance Semiring (RinglikeStruct LeftDistributive RightDistributive Annihilating) where
   getGroup = rStruct1
   getMonoid = rStruct2



m1 = makeGroup (+) (*(-1)) 0 "add"
m2 = makeMonoid (*) 0 "mul"

r1 = RinglikeStruct 0 0 0 m1 m2 "ring"-}

--instance (Group a, Monoid b) => Semiring (r a b) where
--instance (CommutativeGroup a, Monoid b) => Ring (r a b) where
--instance (Ring (r a b), Commutative a) => CommutativeRing (r a b) where

