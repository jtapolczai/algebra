module OrderedAlgebra where

import Grouplike.Internal
import Ringlike.Internal
import Relation.Internal
import Relation.Traits

import Data.List
import Data.Ord

-- |An algebraic structure, combined with a partial
--  order
data OrderedStruct s o el t =
   OrderedStruct{ordStruct::s el t,
                 ordOrder::o el el t,
                 ordTag::t}

getOrdering :: TotalOrder o => o el el t -> el -> el -> Ordering
getOrdering struct a b = if smaller then LT
                         else if greater then GT
                         else EQ
   where smaller = (rel struct) a b
         greater = (rel struct) b a 

mySort :: TotalOrder o => OrderedStruct s o el t -> [el] -> [el]
mySort struct = sortBy (getOrdering $ ordOrder struct)

lt = (RelationStruct 0 0 0 0 ((<)::(Int -> Int -> Bool)) "lt")
intOrd = EndorelationStruct' TagReflexive TagAntisymmetric TagTransitive TagTotal 0 0 0 0 lt
intMag = makeMagma (+) "intAdd"

intOS = OrderedStruct{ordStruct=intMag,ordOrder=intOrd,ordTag="intOS"}