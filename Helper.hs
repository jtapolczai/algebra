{-# LANGUAGE ExistentialQuantification #-}

-- |Contains small helper functions and definitions of little import.
module Helper where

-- |A unary operation.
type Un a = a -> a
-- |A binary relation.
type Bin a = a -> a -> a
-- |A binary predicate.
type Rel a b = a -> b -> Bool

-- |Concatenates a list of elements, putting a separator between each two.
--  @implode x [y1,...,yn] = y1 ++ x ++ y2 ++ ... ++ x ++ yn@
implode :: [a] -> [[a]] -> [a]
implode sep xs | null xs   = []
               | otherwise = foldl1 (\x y -> concat [x,sep,y]) xs

-- |Inserts values into a string.
--  For all @i@, the 0-based @i@th occurrence of @%@ is replaced @list !! i@,
--  where @list@ is the list of elements to insert.
--  If the number of @%@ and the size of @list@ do not match, only
--  occurrences with values available for them will be replaced.
printf :: String -> [String] -> String
printf xs [] = xs
printf [] _ = []
printf ('%':xs) (y:ys) = y ++ printf xs ys
printf (x:xs) ys = x:printf xs ys
