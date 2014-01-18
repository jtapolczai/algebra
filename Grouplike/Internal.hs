{-# LANGUAGE FlexibleInstances #-}

{-|
  Grants access to the internal parts of grouplike structures, allowing
  the circumvention of the static type checks and the querying structural
  properties at runtime.

  Normally, checking structures for desired properties (commutativity, etc.)
  is a purely static action done by the type checker through contexts like
  the following:
  
  @
flipOP :: Commutative a => a el t -> el -> el -> el
flipOP struct = flip (op struct)
  @

  With the functions of this module, the structures can be queried at runtime.
  We can write the same function as:

  @
flipOP :: CommutativityTag c => GrouplikeStruct c a i u l r inv el t -> el -> el -> el
flipOP struct = if isCommutative struct == Just True then flip (op struct)
                else error "Passed non-commutative structure!"
  @

  Here, the grouplike structure is accessed directly, and we had to specify
  that the type variable @c@ representing the structure's commutativity be
  an instance of 'PropertyTag', so that we could apply 'isCommutative' to
  the structure.

  If we need, say, heterogeneous lists of structures, we can bypass the static type-checking
  entirely with the function @makeDynamic@:

  @
[makeDynamic $ makeMonoid (+) 0 "add", makeDynamic $ makeCommutativeMonoid (*) 0]
  @

  The type parameters in the output of makeDynamic are sum types, meaning that, to
  the type checker, there's no difference between commutative/non-commutative, 
  idempotent/non-idempotent, etc. structures, or those with unit elements/inverses/etc.
  and those without.

  Obviously, such access makes programs less safe; on the other hand,
  functions like the following are made possible:

  @
getCommutativeAlgebras :: PropertyTag c => GrouplikeStruct c a i u l r inv el t -> [el] -> [el]
getCommutativeAlgebras = filter (toBool . isCommutative)
  where toBool (Just True) = True
        toBool _           = False
  @

  This takes a list of (dynamic) structures and filters out those which are commutative.

  Another example:

  @
getUnitElements = map getUnitElement
  @

  This again takes a list of structures and returns their unit elements (if they exist).

  Dynamic structures are incompatible with functions that expect statically
  checked ones. For example, the following is not possible:

  @
s = makeDynamic $ makeSemigroup (+) "add"

functionOnGroups :: CommutativeGroup s => s el t -> [el] -> el
functionOnGroups = ...
  @

  >>> f s [...]
  Compile time error

  In order to turn a dynamic structure back into a static one, the it has to
  recreated from scratch via the constructor methods. E.g. to return the monoids
  from a list of dynamic structures, we'd write:

  @
filterMonoids :: [DynamicStruct el t] -> [MonoidStruct el t]
filterMonoids =  makeStatic . (filter (fst . hasUnit . getUnitElemt))
  where hasUnit (UnitElement u) = (True, u) 
        hasUnit _               = (False, undefined)

        makeStatic m = makeMonoid (op m) (snd $ hasUnit $ getUnitElement m) (gTag m)
  @

-}
module Grouplike.Internal (
   module Grouplike.Traits,

   -- * Common grouplike algebraic structure
   GrouplikeStruct(gTag),

   -- * Turning static structures into dynamic ones
   makeDynamic,

   -- * Predefined classes
   Quasigroup,
   Loop,
   Semigroup,
   Monoid,
   CommutativeMonoid,
   Group,
   CommutativeGroup,
   Semilattice,
   BoundedSemilattice,

   -- * Shorthand type synonyms for common structures
   DynamicGroupStruct,
   MagmaStruct,
   QuasigroupStruct,
   LoopStruct,
   SemigroupStruct,
   MonoidStruct,
   CommutativeMonoidStruct,
   GroupStruct,
   CommutativeGroupStruct,
   SemilatticeStruct,
   BoundedSemilatticeStruct,

   -- * Constructors
   makeMagma,
   makeSemigroup,
   makeQuasigroup,
   makeLoop,
   makeMonoid,
   makeCommutativeMonoid,
   makeGroup,
   makeCommutativeGroup,
   makeSemilattice,
   makeBoundedSemilattice,

   -- * Adding traits to structures
   addCommutativity,
   addAssociativity,
   addIdempotence,
   addUnitElement,
   addLeftDivider,
   addRightDivider,
   addInverse,

   -- * Checking for the presence of traits
   isCommutative,
   isAssociative,
   isIdempotent,
   getUnitElement,
   getLeftDivider,
   getRightDivider,
   getDividers,
   getInverse
   ) where

import Control.Applicative
import Grouplike.Traits
import Helper
import Templates


-- |A magma with left and right dividers.
class (LeftDivisible s, RightDivisible s) => Quasigroup s where
-- |A quasigroup with a unit element.
class (Quasigroup s, HasUnitElement s) => Loop s where
-- |An associative magma.
class Associative s => Semigroup s where
-- |A semigroup with a unit element.
class (Semigroup s, HasUnitElement s) => Monoid s where
-- |A commutative monoid.
class (Monoid s, Commutative s) => CommutativeMonoid s where
-- |A magma which is both a monoid and a loop. Every element has an inverse.
class (Loop s, Monoid s, Invertible s) => Group s where
-- |A commutative group.
class (CommutativeMonoid s, Group s) => CommutativeGroup s where
-- |A commutative and idempotent semigroup. Its operation is commonly the meet or the join or two elements.
class (Semigroup s, Commutative s, Idempotent s) => Semilattice s
-- |A semilattice with a unit element.
class (Semilattice s, HasUnitElement s) => BoundedSemilattice s

-- |A general grouplike structure which underlies
--  all the special cases (Semigroups, Monoids, etc.)
data GrouplikeStruct c a i u l r inv el t = 
  GrouplikeStruct{gCommutativity::c,
                  gAssociativity::a,
                  gIdempotence::i,
                  gUnitElement::u el,
                  gLeftDivider::l (Bin el),
                  gRightDivider::r (Bin el),
                  gInverse::inv (Un el),
                  gOperation::Bin el,
                  -- |Gets the tag of the structure.
                  gTag::t}

-- Show-instance for GrouplikeStruct

instance (CommutativityTag c,
          AssociativityTag a,
          IdempotenceTag i,
          UnitElementTag u,
          LeftDividerTag l,
          RightDividerTag r,
          InverseTag inv,
          Show el,
          Show t) => Show (GrouplikeStruct c a i u l r inv el t) where
   show struct = "Grouplike [" ++ show t ++ "] (" ++ implode ", " traits ++ ")"
      where cm = getCommutativityValue $ gCommutativity struct
            as = getAssociativityValue $ gAssociativity struct
            ie = getIdempotenceValue $ gIdempotence struct
            ui = getUnitElementValue $ gUnitElement struct
            ld = getLeftDividerValue $ gLeftDivider struct
            rd = getRightDividerValue $ gRightDivider struct
            iv = getInverseValue $ gInverse struct
            t = gTag struct
            traits = filter (not . null) $ [show cm, show as, show ie, show ui, show ld, show rd, show iv]


-- Instances for primitive traits

instance Grouplike (GrouplikeStruct c a i u l r inv) where
  op = gOperation
instance Commutative (GrouplikeStruct TagCommutative a i u l r inv) where
instance Associative (GrouplikeStruct c TagAssociative i u l r inv) where
instance Idempotent (GrouplikeStruct c a TagIdempotent u l r inv) where
instance HasUnitElement (GrouplikeStruct c a i TagUnitElement l r inv) where
   ident = (\(TagUnitElement a) -> a) . gUnitElement
instance LeftDivisible (GrouplikeStruct c a i u TagLeftDivider r inv) where
   lDiv = (\(TagLeftDivider a) -> a) . gLeftDivider
instance RightDivisible (GrouplikeStruct c a i u l TagRightDivider inv) where
   rDiv = (\(TagRightDivider a) -> a) . gRightDivider
instance Divisible (GrouplikeStruct c a i u TagLeftDivider TagRightDivider inv) where
instance Invertible (GrouplikeStruct c a i TagUnitElement l r TagInverse) where
   inverse = (\(TagInverse a) -> a) . gInverse

-- Instances for algebraic structures

instance Quasigroup (GrouplikeStruct c a i u TagLeftDivider TagRightDivider inv) where
instance Semigroup (GrouplikeStruct c TagAssociative i u l r inv) where
instance Loop (GrouplikeStruct c a i TagUnitElement TagLeftDivider TagRightDivider inv) where
instance Monoid (GrouplikeStruct c TagAssociative i TagUnitElement l r inv) where
instance CommutativeMonoid (GrouplikeStruct TagCommutative TagAssociative i TagUnitElement l r inv) where
instance Group (GrouplikeStruct c TagAssociative i TagUnitElement TagLeftDivider TagRightDivider TagInverse) where
instance CommutativeGroup (GrouplikeStruct TagCommutative TagAssociative i TagUnitElement TagLeftDivider TagRightDivider TagInverse) where
instance Semilattice (GrouplikeStruct TagCommutative TagAssociative TagIdempotent u l r inv) where
instance BoundedSemilattice (GrouplikeStruct TagCommutative TagAssociative TagIdempotent TagUnitElement l r inv) where

-- |Type synonym for a dynamic structure which doesn't have static checks on its properties.
type DynamicGroupStruct el t = GrouplikeStruct CommutativityValue AssociativityValue IdempotenceValue UnitElementValue LeftDividerValue RightDividerValue InverseValue el t

-- |Type synomym for a magma.
--type MagmaStruct el t = GrouplikeStruct TagUnknownCommutative TagUnknownAssociative TagUnknownIdempotent TagUnknownUnitElement TagUnknownLeftDivider TagUnknownRightDivider TagUnknownInverse el t
$(makeStructureTypeSynonym' "MagmaStruct" "GrouplikeStruct" [] grouplikeTraits ["el", "t"])

-- |Type synonym for a quasigroup.
$(makeStructureTypeSynonym' "QuasigroupStruct" "GrouplikeStruct" ["LeftDivider", "RightDivider"] grouplikeTraits ["el", "t"])
-- |Type synonym for a semigroup.
$(makeStructureTypeSynonym' "SemigroupStruct" "GrouplikeStruct" ["Associative"] grouplikeTraits ["el", "t"])
-- |Type synonym for a loop.
$(makeStructureTypeSynonym' "LoopStruct" "GrouplikeStruct" ["UnitElement", "LeftDivider", "RightDivider"] grouplikeTraits ["el", "t"])
-- |Type synonym for a monoid.
$(makeStructureTypeSynonym' "MonoidStruct" "GrouplikeStruct" ["Associative", "UnitElement"] grouplikeTraits ["el", "t"])
-- |Type synonym for a commutative monoid.
$(makeStructureTypeSynonym' "CommutativeMonoidStruct" "GrouplikeStruct" ["Commutative", "Associative", "UnitElement"] grouplikeTraits ["el", "t"])
-- |Type synonym for a group.
$(makeStructureTypeSynonym' "GroupStruct" "GrouplikeStruct" ["Associative", "UnitElement", "LeftDivider", "RightDivider", "Inverse"] grouplikeTraits ["el", "t"])
-- |Type synonym for a commutative group.
$(makeStructureTypeSynonym' "CommutativeGroupStruct" "GrouplikeStruct" ["Commutative", "Associative", "UnitElement", "LeftDivider", "RightDivider", "Inverse"] grouplikeTraits ["el", "t"])
-- |Type synonym for a semilattice.
$(makeStructureTypeSynonym' "SemilatticeStruct" "GrouplikeStruct" ["Commutative", "Associative", "Idempotent"] grouplikeTraits ["el", "t"])
-- |Type synonym for a bounded semilattice.
$(makeStructureTypeSynonym' "BoundedSemilatticeStruct" "GrouplikeStruct" ["Commutative", "Associative", "Idempotent", "UnitElement"] grouplikeTraits ["el", "t"])

-- Addition of individual traits to a structure

-- |Adds the promise of commutativity to a structure.
addCommutativity :: GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct TagCommutative a i u l r inv el t
addCommutativity (GrouplikeStruct _ a i u l r inv el t) = GrouplikeStruct TagCommutative a i u l r inv el t

-- |Adds the promise of associativity to a structure.
addAssociativity :: GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c TagAssociative i u l r inv el t
addAssociativity (GrouplikeStruct c _ i u l r inv el t) = GrouplikeStruct c TagAssociative i u l r inv el t

-- |Adds the promise of idempotence to a structure.
addIdempotence :: GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c a TagIdempotent u l r inv el t
addIdempotence (GrouplikeStruct c a _ u l r inv el t) = GrouplikeStruct c a TagIdempotent u l r inv el t

-- |Adds a unit element to a structure.
addUnitElement :: el -> GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c a i TagUnitElement l r inv el t
addUnitElement ui (GrouplikeStruct c a i _ l r inv el t) = GrouplikeStruct c a i (TagUnitElement ui) l r inv el t

-- |Adds a left divider to a structure.
addLeftDivider :: Bin el -> GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c a i u TagLeftDivider r inv el t
addLeftDivider ld (GrouplikeStruct c a i u _ r inv el t) = GrouplikeStruct c a i u (TagLeftDivider ld) r inv el t

-- |Adds a right divider to a structure.
addRightDivider :: Bin el -> GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c a i u l TagRightDivider inv el t
addRightDivider rd (GrouplikeStruct c a i u l _ inv el t) = GrouplikeStruct c a i u l (TagRightDivider rd) inv el t

-- |Adds an invere function to a structure.
addInverse :: Un el -> GrouplikeStruct c a i u l r inv el t -> GrouplikeStruct c a i u l r TagInverse el t
addInverse i' (GrouplikeStruct c a i u l r _ el t) = GrouplikeStruct c a i u l r (TagInverse i') el t


-- Creation of structures

-- |Creates a magma.
makeMagma :: Bin el -- ^The structure's binary operation.
          -> t      -- ^The structure's tag.
          -> MagmaStruct el t
makeMagma = GrouplikeStruct TagUnknownCommutative TagUnknownAssociative TagUnknownIdempotent TagUnknownUnitElement TagUnknownLeftDivider TagUnknownRightDivider TagUnknownInverse

-- |Creates a quasigroup.
makeQuasigroup :: Bin el -- ^The structure's binary operation.
               -> Bin el -- ^Left divider.
               -> Bin el -- ^Right divider.
               -> t      -- ^The structure's tag.
               -> QuasigroupStruct el t
makeQuasigroup o ld rd t = addRightDivider rd $ addLeftDivider ld $ makeMagma o t

-- |Creates a semigroup.
makeSemigroup :: Bin el -- ^The structure's binary operation.
              -> t      -- ^The structure's tag.
              -> SemigroupStruct el t
makeSemigroup o t = addAssociativity $ makeMagma o t

-- |Creates a loop.
makeLoop :: Bin el -- ^The structure's binary operation.
         -> Bin el -- ^Left divider.
         -> Bin el -- ^Right divider.
         -> el     -- ^Unit element.
         -> t      -- ^The structure's tag.
         -> LoopStruct el t
makeLoop o ld rd ui t = addUnitElement ui $ makeQuasigroup o ld rd t

-- |Creates a monoid.
makeMonoid :: Bin el -- ^The structure's binary operation.
           -> el     -- ^Unit element.
           -> t      -- ^The structure's tag.
           -> MonoidStruct el t
makeMonoid o ui t = addUnitElement ui $ makeSemigroup o t

-- |Creates a commutative monoid.
makeCommutativeMonoid :: Bin el -- ^The structure's binary operation.
           -> el                 -- ^Unit element.
           -> t                  -- ^The structure's tag.
           -> CommutativeMonoidStruct el t
makeCommutativeMonoid o ui t = addCommutativity $ makeMonoid o ui t

-- |Creates a group.
makeGroup :: Bin el -- ^The structure's binary operation (op).
          -> Un el  -- ^Inverse function inv. It also specifies the left and right dividers as: leftDivider a b = inv a `op` b; rightDivider a b = b `op` inv a
          -> el     -- ^Unit element.
          -> t      -- ^The structure's tag.
          -> GroupStruct el t
makeGroup o inv ui t = addInverse inv $ addRightDivider rd $ addLeftDivider ld $ makeMonoid o ui t
   where ld a b = inv a `o` b
         rd a b = b `o` inv a

-- |Creates a commutative group.
makeCommutativeGroup :: Bin el -- ^The structure's binary operation (op).
          -> Un el  -- ^Inverse function inv. It also specifies the left and right dividers as: leftDivider a b = inv a `op` b; rightDivider a b = b `op` inv a
          -> el     -- ^Unit element.
          -> t      -- ^The structure's tag.
          -> CommutativeGroupStruct el t
makeCommutativeGroup o inv ui t = addCommutativity $ makeGroup o inv ui t

-- |Creates a semilattice.
makeSemilattice :: Bin el -- ^The structure's binary operation.
                -> t      -- ^The structure's tag.
                -> SemilatticeStruct el t
makeSemilattice o t = addIdempotence $ addCommutativity $ makeSemigroup o t

-- |Creates a bounded semilattice.
makeBoundedSemilattice :: Bin el -- ^The structure's binary operation.
                       -> el     -- ^Unit element.
                       -> t      -- ^The structure's tag.
                       -> BoundedSemilatticeStruct el t
makeBoundedSemilattice o ui t = addUnitElement ui $ makeSemilattice o t


-- |Turns a statically typechecked grouplike structure into a one without it.
--  Whereas the values the type parameters can take in the input will be unrelated
--  (e.g. @TagCommutative@ and @TagNonCommutative@ are distinct and incompatible types),
--  the type parameters int the output will belong to sum types
--  (@Commutative@ and @NonCommutative@ are both constructors of @CommutativityValue@).
--  The resulting structure will thereby be able to bypass all static type checks and
--  be accepted anywhere where a (dynamic) grouplike structure is accepted.
makeDynamic :: (CommutativityTag c,
                AssociativityTag a,
                IdempotenceTag i,
                UnitElementTag u,
                LeftDividerTag l,
                RightDividerTag r,
                InverseTag inv)
            => GrouplikeStruct c a i u l r inv el t -> DynamicGroupStruct el t
makeDynamic g = (GrouplikeStruct (isCommutative g)
                                 (isAssociative g)
                                 (isIdempotent g)
                                 (getUnitElement g)
                                 (getLeftDivider g)
                                 (getRightDivider g)
                                 (getInverse g)
                                 (gOperation g)
                                 (gTag g))

-- Typesafe accessor functions for structures

-- |Returns whether the structure is commutative.
isCommutative :: (CommutativityTag c) => GrouplikeStruct c a i u l r inv el t -> CommutativityValue
isCommutative = getCommutativityValue . gCommutativity

-- |Returns whether the structure is associative.
isAssociative :: (AssociativityTag a) => GrouplikeStruct c a i u l r inv el t -> AssociativityValue
isAssociative = getAssociativityValue . gAssociativity

-- |Returns whether the structure is idempotent.
isIdempotent :: (IdempotenceTag i) => GrouplikeStruct c a i u l r inv el t -> IdempotenceValue
isIdempotent = getIdempotenceValue . gIdempotence

-- |Returns the unit element of the structure, if it exists.
getUnitElement :: (UnitElementTag u) => GrouplikeStruct c a i u l r inv el t -> UnitElementValue el
getUnitElement = getUnitElementValue . gUnitElement

-- |Returns the left divider function of the structure, if it exists.
getLeftDivider :: (LeftDividerTag l) => GrouplikeStruct c a i u l r inv el t -> LeftDividerValue (Bin el)
getLeftDivider = getLeftDividerValue . gLeftDivider

-- |Returns the right divider function of the structure, if it exists.
getRightDivider :: (RightDividerTag r) => GrouplikeStruct c a i u l r inv el t -> RightDividerValue (Bin el)
getRightDivider = getRightDividerValue . gRightDivider

-- |Returns the left divider and right divider functions of the structure, if they exists.
getDividers :: (LeftDividerTag l, RightDividerTag r) => GrouplikeStruct c a i u l r inv el t -> (LeftDividerValue (Bin el), RightDividerValue (Bin el))
getDividers = liftA2 (,) getLeftDivider getRightDivider

-- |Returns the inverse function of the structure, if it exists.
getInverse :: (InverseTag inv) => GrouplikeStruct c a i u l r inv el t -> InverseValue (Un el)
getInverse = getInverseValue . gInverse
