{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ringlike.Internal where

import Grouplike.Internal
import Ringlike.Traits

--absorption (into grouplikes?)

data RinglikeStruct dl dr an ja bi eli g1 g2 el t1 t2 =
   RinglikeStruct{rLeftDistributive::dl,
                  rRightDistributive::dr,
                  rAnnihiliate::an,
                  rJacobi::ja,
                  rBilinear::bi,
                  rEliminating::eli,
                  rStruct1::g1 el t1,
                  rStruct2::g2 el t2,
                  rTag::(t1,t2)}


-- TODO: clear up this chaos

class (CommutativeMonoid a, Monoid b, Distributive r, Annihilating r) => Semiring r a b
class (Group a, Semigroup b, RightDistributive r) => RightNearRing r a b
class (Group a, Semigroup b, LeftDistributive r) => LeftNearRing r a b
class (CommutativeGroup a, Semiring r a b) => Ring r a b
class (CommutativeMonoid a, Ring r a b) => CommutativeRing r a b
class (CommutativeGroup a, Anticommutative b, Distributive r, JacobiIdentity r,
       Bilinear r, Eliminating r) => LieRing r a b
class (Semilattice a, Semilattice b, Ring r a b) => BooleanRing r a b

-- TODO: insert to full constraints on these

class (CommutativeRing r a b) => IntegralDomain r a b 
class (IntegralDomain r a b) => IntegrallyClosedDomain r a b
class (IntegrallyClosedDomain r a b) => UniqueFactorizationDomain r a b
class (UniqueFactorizationDomain r a b) => PrincipalIdealDomain r a b
class (PrincipalIdealDomain r a b) => EuclideanDomain r a b
class (EuclideanDomain r a b) => Field r a b
class (Field r a b) => FiniteField r a b


instance Ringlike (RinglikeStruct dl dr an ja bi eli) where
   getStruct1 = rStruct1
   getStruct2 = rStruct2

instance LeftDistributive (RinglikeStruct TagLeftDistributive dr an ja bi eli)
instance RightDistributive (RinglikeStruct dl TagRightDistributive an ja bi eli)
instance Distributive (RinglikeStruct TagLeftDistributive TagRightDistributive an ja bi eli)
instance Annihilating (RinglikeStruct dl dr TagAnnihilating ja bi eli)
instance JacobiIdentity (RinglikeStruct dl dr an TagJacobiIdentity bi eli)
instance Bilinear (RinglikeStruct dl dr an ja TagBilinear eli)
instance Eliminating (RinglikeStruct dl dr an ja bi TagEliminating)

instance (CommutativeMonoid a, Monoid b) => Semiring (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja bi eli) a b
instance (Group a, Semigroup b) => RightNearRing (RinglikeStruct dl TagRightDistributive an ja bi eli) a b
instance (Group a, Semigroup b) => LeftNearRing (RinglikeStruct TagLeftDistributive dr an ja bi eli) a b
instance (CommutativeGroup a, Monoid b) => Ring (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja bi eli) a b
instance (CommutativeGroup a, CommutativeMonoid b) => CommutativeRing (RinglikeStruct TagLeftDistributive TagRightDistributive TagAnnihilating ja bi eli) a b

--class (CommutativeGroup a, Monoid b) => Ring r a b where
--   getSR1 :: r a b el t1 t2 -> (a el t1)

--instance (CommutativeGroup a, Monoid b) => Ring (RinglikeStruct dl dr an ja bi eli) a b where
--   getSR1 = rStruct1

--f :: (Ring r a b, Commutative b) => r a b el t1 t2 -> [el] -> el
--f r = foldl o i
--   where o = op $ getSR1 r
--         i = ident $ getSR1 r

--r1 = RinglikeStruct 0 0 0 (makeCommutativeGroup (+) undefined 0 "add") (makeCommutativeMonoid (*) 1 "mult") ("add","mult")

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

