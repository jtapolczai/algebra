module Helper where

-- |A unary operation.
type Un a = a -> a
-- |A binary relation.
type Bin a = a -> a -> a
-- |A binary predicate.
type Rel a = a -> a -> Bool

implode :: [a] -> [[a]] -> [a]
implode sep xs | null xs   = []
               | otherwise = foldl1 (\x y -> concat [x,sep,y]) xs