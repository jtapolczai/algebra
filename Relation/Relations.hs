module Relations where

data RelationStruct r tr s tot tag =
   RelationStruct{relReflexivity::r,
                  relTransitivity::tr,
                  relSymmetry::s,
                  relTotality::tot,
                  relComp::el -> el -> Bool,
                  }


