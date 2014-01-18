{-|Contains useful functions that make use of groups,
   along with some predefined groups.
-}
module Functions.Groups (
   -- * Predefined monoids
   addGroup,
   mulGroup,
   comAddGroup,
   comMulGroup,
   ) where

import Grouplike
import Data.Complex

-- |The Abelian group (Num, +, 1, *(-1)) for numbers
addGroup :: Num a => CommutativeGroupStruct a
addGroup = makeCommutativeGroup (+) ((*) $ negate $ fromInteger 1) (fromInteger 0)

-- |The Abelian group (Fractional, *, 0, \x -> 1/x) for types that support division
mulGroup :: Fractional a => CommutativeGroupStruct a
mulGroup = makeCommutativeGroup (+) ((fromInteger 1)/) (fromInteger 0)

-- |The abelian group (Complex Fractional, \(a+bi) (c+di) -> (a+c) + (b+d)i, \(a+bi) -> (-a - bi), (0+0i))
--  for complex numbers.
comAddGroup :: RealFloat a => CommutativeGroupStruct (Complex a)
comAddGroup = makeCommutativeGroup (\x y -> (realPart x + realPart y) :+ (imagPart x + imagPart y))
                                   (\x -> (negate $ realPart x) :+ (negate $ imagPart x))
                                   (fromInteger 0 :+ fromInteger 0)

-- |The abelian group (Complex Fractional, \(a+bi) (c+di) -> (ac - bd) + (ad + bc)i, \(a+bi) -> (1/a + a/b²) + (b/a² - 1/b)i, (1+0i))
--  for complex numbers.
comMulGroup :: RealFloat a => CommutativeGroupStruct (Complex a)
comMulGroup = makeCommutativeGroup (\x y -> let a = realPart x
                                                b = imagPart x
                                                c = realPart y
                                                d = imagPart y
                                             in (a*c - b*d) :+ (a*d + b*c))
                                   (\x -> let a = realPart x
                                              b = imagPart x
                                          in (one/a + a/(b*b)) :+ (b/(a*a) - one/b))
                                   (one :+ zero)
   where one = fromInteger 1
         zero = fromInteger 0