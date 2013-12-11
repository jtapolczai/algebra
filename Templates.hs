{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Templates (makeContentTag, makeEnumTag) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax


{-| Generates the code for a simple trait which is just an enumeration,
    which consists of a sum type (enumaration) of the possible values,
    along with a (non-sum) type for each of the values.

    The non-sum types can be passed to algebraic structures as parameters in order
    to distinguish, say, commutative structures from noncommutative ones.
    The sum type exists to enable determining the properties of a structure dynamically
    at run-type, and comes with a class (which all non-sum types implement) which transforms
    the non-sum types into the sum type.

    The first parameter is the tag name, the second is the enumeration
    of possible tag value. The generated generated is:

    @
      (The sum type of possible values)
      data [TagName]Value = [TagValue0] | ... | [TagValueN] deriving (Eq, Show, Read)

      (The non-sum types of possible values)
      data Tag[TagValue0] = Tag[TagValue0] deriving (Eq, Show, Read)
      ...
      data Tag[TagValueN] = Tag[TagValueN] deriving (Eq, Show, Read)

      (The class for turning the non-sum types into instances of the sum type)
      class [TagName]Tag t where
         get[TagName]Value :: t -> [TagName]Value

      (The instance declarations for the non-sum types)
      instance [TagName]Tag [TagValue0] where get[TagName]Value _ = [TagValue0]
      ...
      instance [TagName]Tag [TagValue0] where get[TagName]Value _ = [TagValue0]
    
      (The instance declaration for the sum type)
      instance [TagName]Tag [TagName]Value where get[TagName]Value = id
    @

    An simple example for trait A and values X, Y would be (deriving-claused ommitted):

    @
      data AValue = X | Y

      data TagX = TagX
      data TagY = TagY

      class XTag t where
         getAValue :: t -> AValue

      instance XTag TagX where getA _ = X
      instance XTag TagY where getA _ = Y

      instance XTag AValue where getA = id
    @

-}
makeEnumTag :: String -> [String] -> DecsQ
makeEnumTag tagName = makeContentTag tagName . map (\x -> (x,False))


{-| Generates the code for a trait where some values may have data attached.
    The traits consists of a sum type (enumaration) of the possible values,
    along with a (non-sum) type for each of the values.

    The non-sum types can be passed to algebraic structures as parameters in order
    to distinguish, say, commutative structures from noncommutative ones.
    The sum type exists to enable determining the properties of a structure dynamically
    at run-type, and comes with a class (which all non-sum types implement) which transforms
    the non-sum types into the sum type.

    The first parameter is the tag name, the second is the enumeration
    of possible tag values, together with a Bool indicating whether that
    value has attached data. The generated code is:

    @
      (The sum type of possible values)
      data [TagName]Value = [TagValue0] [optional data] | ... | [TagValueN] [optional data] deriving (Eq, Show, Read)

      (The non-sum types of possible values)
      data Tag[TagValue0] [optional data] = Tag[TagValue0] [optional data] deriving (Eq, Show, Read)
      ...
      data Tag[TagValueN] [optional data] = Tag[TagValueN] [optional data] deriving (Eq, Show, Read)

      (The class for turning the non-sum types into instances of the sum type)
      class [TagName]Tag t where
         get[TagName]Value :: t [a if at least one constructor has data] -> [TagName]Value

      (The instance declarations for the non-sum types)
      instance [TagName]Tag [TagValue0] where get[TagName]Value (Tag[TagValue0] [a]) = [TagValue0] [a]
      ...
      instance [TagName]Tag [TagValueN] where get[TagName]Value (Tag[TagValuN0] [a]) = [TagValueN] [a]
    
      (The instance declaration for the sum type)
      instance [TagName]Tag [TagName]Value where get[TagName]Value = id
    @

    An example is the trait "UnitElement", where one constructor has data (the unit element),
    and the other does not:

    @
      $(makeContentTag "UnitElement" [("HasUnit", True), ("NoUnit", False)])
    @

    @
      data UnitElementValue a = HasUnit a | NoUnit

      data TagHasUnit a = TagHasUnit a
      data TagNoUnit = TagNoUnit

      class UnitElementTag t where
        (since at least one constructor has data, the type is "t a -> ..." instead of "t -> ...")
         getUnitElementValue :: t a -> UnitElementValue

      instance UnitElementTag TagX where getUnitElementValur _ = X
      instance UnitElemenTag TagY where getUnitElementValur _ = Y

      instance UnitElementTag UnitElementValue where getUnitElementValue = id
    @

-}
makeContentTag :: String -> [(String,Bool)] -> DecsQ
makeContentTag tagName tagValues = sequenceQ $ sumType : nonSumTypes ++ [tagClass] ++ (sumTypeInstance : instances)
   where cxt' = returnQ []

         typeParamNeeded = or $ map snd $ tagValues
         aTV = if typeParamNeeded then [PlainTV $ mkName "a"] else []
         aType = if typeParamNeeded then [strictType notStrict $ varT $ mkName "a"] else []

         --sum type
         stName = mkName $ tagName ++ "Value"
         sumType = dataD cxt' stName aTV sumTypeConstructors derived

         mkTypeConstructor (x, True) = normalC (mkName x) aType
         mkTypeConstructor (x, False) = normalC (mkName x) []

         sumTypeConstructors = map mkTypeConstructor tagValues

         --non-sum types
         mkNonSumType (x,b) = dataD cxt' (mkName $ "Tag" ++ x) aTV
                                    [mkTypeConstructor ("Tag"++x, b)] derived
         nonSumTypes = map mkNonSumType tagValues

         derived = map mkName ["Eq"]

         --class declaration
         t = varT $ mkName "t"
         methodName = mkName $ "get" ++ tagName ++ "Value"
         className = mkName $ tagName ++ "Tag"

         methodType = if typeParamNeeded then forallT aTV cxt' $ appT (appT arrowT (appT t (varT $ mkName "a"))) (appT (conT stName) (varT $ mkName "a"))
                      else appT (appT arrowT t) (conT stName)

         getMethod = sigD methodName methodType
         tagClass = classD cxt' className [PlainTV $ mkName "t"] [] [getMethod]
         
         --instance declarations
         makeInstance (x,b) = instanceD cxt' (appT (conT $ className) (conT $ mkName $ "Tag" ++ x)) [methodImpl (x,b)]
         methodImpl (x,b) = funD methodName (if b then [clause [conP (mkName ("Tag" ++ x)) [varP $ mkName "a"]]
                                                (normalB $ appE (conE $ mkName x) (varE $ mkName "a")) []]
                                             else [clause [wildP] (normalB $ conE $ mkName x) []])
         instances = map makeInstance tagValues

         sumTypeInstance = instanceD cxt' (appT (conT $ className) (conT $ mkName $ tagName ++ "Value")) [sumTypeImpl]
         sumTypeImpl = funD methodName [clause [] (normalB $ varE $ mkName "id") []]

-- |Prints the code generated by a template to the console.
printCode :: Ppr a => Q a -> IO ()
printCode c = do
   c' <- runQ c
   putStrLn $ pprint c'


