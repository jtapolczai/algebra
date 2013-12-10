module Helper where

-- |A unary operation.
type Un a = a -> a
-- |A binary relation.
type Bin a = a -> a -> a
-- |A binary predicate.
type Rel a = a -> a -> Bool