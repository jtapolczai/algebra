{-|Contains useful functions that make use of monoids,
   along with some predefined monoids.
-}
module Functions.Monoids (
   -- * Predefined monoids
   andMonoid,
   orMonoid,
   mulMonoid,
   listMonoid,
   funcMonoid,
   unionMonoid,
   maybeMonoid,
   monadMonoid,

   -- * Functions on monoids
   mdsum,
   mdsum'
   ) where

import Prelude hiding (foldl)
import Data.Foldable
import qualified Data.Set as Set

import Grouplike

-- |The commutative monoid @(Bool, &&, True)@
andMonoid :: CommutativeMonoidStruct Bool
andMonoid = makeCommutativeMonoid (&&) True

-- |The commutative monoid @(Bool, ||, False)@
orMonoid :: CommutativeMonoidStruct Bool
orMonoid = makeCommutativeMonoid (||) False

-- |The commutative monoid @(Num, *, 0)@ for numbers
mulMonoid :: Num a => CommutativeMonoidStruct a
mulMonoid = makeCommutativeMonoid (*) (fromInteger 0)

-- |The monoid @([a], ++, [])@ for lists.
listMonoid :: MonoidStruct [a]
listMonoid = makeMonoid (++) []

-- |The monoid @((a -> a), (.), id)@ for functions.
funcMonoid :: MonoidStruct (a -> a)
funcMonoid = makeMonoid (.) id

-- |The commutative monoid @(Set a, `union`, empty)@ for sets.
unionMonoid :: Ord a => CommutativeMonoidStruct (Set.Set a)
unionMonoid = makeCommutativeMonoid (Set.union) (Set.empty)

-- |The monoid @(Maybe el, f, Nothing)@ for Maybe el which wraps an existing
--  monoid into a Maybe.
--  
--  Let's call the wrapped monoid's operation +. @f x y@ is defined as
--
--  @ 
--f (Just x) (Just y) = Just $ x + y
--f _ _               = Nothing
--  @
maybeMonoid :: Monoid a => a el -> MonoidStruct (Maybe el)
maybeMonoid m = makeMonoid f Nothing
  where f (Just x) (Just y) = Just $ (op m) x y
        f _ _               = Nothing

-- |The monoid @(m, bind, return)@, where @m@ is a monad,
--  @bind@ is defined as
--
--  @
--bind f g = \x -> (g x >>= f)
--  @
--
-- This wraps monads inside a monoid, where the elements
-- are of type @a -> m a@, and the operation is @>>=@, slightly changed
-- to have the type @(a -> m a) -> (a -> m a) -> (a -> m a)@.
monadMonoid :: Monad m => MonoidStruct (a -> m a)
monadMonoid = makeMonoid bind return
  where bind f g = \x -> (g x >>= f)

-- |Calculates the sum of a collection of elements.
mdsum :: (Monoid a, Foldable l)
     => a el -- ^The monoid structure with which to sum.
     -> l el -- ^A foldable collection of elements.
     -> el -- ^The sum of the elements.
mdsum struct = foldl (op struct) (ident struct)

-- |The strict version of msum.
mdsum' :: (Monoid a, Foldable l)
     => a el -- ^The monoid structure with which to sum.
     -> l el -- ^A foldable collection of elements.
     -> el -- ^The sum of the elements.
mdsum' struct = foldl' (op struct) (ident struct)

