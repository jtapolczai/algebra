{-# LANGUAGE FlexibleInstances #-}

{-|
  Grants access to the internal parts of grouplike structures, allowing
  the circumvention of the static type checks and the querying structural
  properties at runtime.

  Normally, checking structures for desired properties (commutativity, etc.)
  is a purely static action done by the type checker through contexts like
  the following:
  
  @
flipOP :: Commutative a => a el -> el -> el -> el
flipOP struct = flip (op struct)
  @

  With the functions of this module, the structures can be queried at runtime.
  We can write the same function as:

  @
flipOP :: CommutativityTag c => GrouplikeStruct c a i u l r inv el -> el -> el -> el
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
[makeDynamic $ makeMonoid (+) 0, makeDynamic $ makeCommutativeMonoid (*) 0]
  @

  The type parameters in the output of makeDynamic are sum types, meaning that, to
  the type checker, there's no difference between commutative/non-commutative, 
  idempotent/non-idempotent, etc. structures, or those with unit elements/inverses/etc.
  and those without.

  Obviously, such access makes programs less safe; on the other hand,
  functions like the following are made possible:

  @
getCommutativeAlgebras :: PropertyTag c => GrouplikeStruct c a i u l r inv el -> [el] -> [el]
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
s = makeDynamic $ makeSemigroup (+)

functionOnGroups :: CommutativeGroup s => s el -> [el] -> el
functionOnGroups = ...
  @

  >>> f s [...]
  Compile time error

  In order to turn a dynamic structure back into a static one, the it has to
  recreated from scratch via the constructor methods. E.g. to return the monoids
  from a list of dynamic structures, we'd write:

  @
filterMonoids :: [DynamicStruct el] -> [MonoidStruct el]
filterMonoids =  makeStatic . (filter (fst . hasUnit . getUnitElemt))
  where hasUnit (UnitElement u) = (True, u) 
        hasUnit _               = (False, undefined)

        makeStatic m = makeMonoid (op m) (snd $ hasUnit $ getUnitElement m)
  @

-}
module Grouplike.Internal (
   module Grouplike.Traits,

   -- * Common grouplike algebraic structure
   GrouplikeStruct,

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
data GrouplikeStruct c a i u l r inv el = 
  GrouplikeStruct{gCommutativity::c,
                  gAssociativity::a,
                  gIdempotence::i,
                  gUnitElement::u el,
                  gLeftDivider::l (Bin el),
                  gRightDivider::r (Bin el),
                  gInverse::inv (Un el),
                  gOperation::Bin el}

-- Show-instance for GrouplikeStruct

instance (CommutativityTag c,
          AssociativityTag a,
          IdempotenceTag i,
          UnitElementTag u,
          LeftDividerTag l,
          RightDividerTag r,
          InverseTag inv,
          Show el) => Show (GrouplikeStruct c a i u l r inv el) where
   show struct = "Grouplike (" ++ implode ", " traits ++ ")"
      where cm = getCommutativityValue $ gCommutativity struct
            as = getAssociativityValue $ gAssociativity struct
            ie = getIdempotenceValue $ gIdempotence struct
            ui = getUnitElementValue $ gUnitElement struct
            ld = getLeftDividerValue $ gLeftDivider struct
            rd = getRightDividerValue $ gRightDivider struct
            iv = getInverseValue $ gInverse struct
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
type DynamicGroupStruct el = GrouplikeStruct CommutativityValue AssociativityValue IdempotenceValue UnitElementValue LeftDividerValue RightDividerValue InverseValue el

-- |Type synomym for a magma.
$(makeStructureTypeSynonym' "MagmaStruct" "GrouplikeStruct" [] grouplikeTraits ["el"])

-- |Type synonym for a quasigroup.
$(makeStructureTypeSynonym' "QuasigroupStruct" "GrouplikeStruct" ["LeftDivider", "RightDivider"] grouplikeTraits ["el"])
-- |Type synonym for a semigroup.
$(makeStructureTypeSynonym' "SemigroupStruct" "GrouplikeStruct" ["Associative"] grouplikeTraits ["el"])
-- |Type synonym for a loop.
$(makeStructureTypeSynonym' "LoopStruct" "GrouplikeStruct" ["UnitElement", "LeftDivider", "RightDivider"] grouplikeTraits ["el"])
-- |Type synonym for a monoid.
$(makeStructureTypeSynonym' "MonoidStruct" "GrouplikeStruct" ["Associative", "UnitElement"] grouplikeTraits ["el"])
-- |Type synonym for a commutative monoid.
$(makeStructureTypeSynonym' "CommutativeMonoidStruct" "GrouplikeStruct" ["Commutative", "Associative", "UnitElement"] grouplikeTraits ["el"])
-- |Type synonym for a group.
$(makeStructureTypeSynonym' "GroupStruct" "GrouplikeStruct" ["Associative", "UnitElement", "LeftDivider", "RightDivider", "Inverse"] grouplikeTraits ["el"])
-- |Type synonym for a commutative group.
$(makeStructureTypeSynonym' "CommutativeGroupStruct" "GrouplikeStruct" ["Commutative", "Associative", "UnitElement", "LeftDivider", "RightDivider", "Inverse"] grouplikeTraits ["el"])
-- |Type synonym for a semilattice.
$(makeStructureTypeSynonym' "SemilatticeStruct" "GrouplikeStruct" ["Commutative", "Associative", "Idempotent"] grouplikeTraits ["el"])
-- |Type synonym for a bounded semilattice.
$(makeStructureTypeSynonym' "BoundedSemilatticeStruct" "GrouplikeStruct" ["Commutative", "Associative", "Idempotent", "UnitElement"] grouplikeTraits ["el"])

-- Addition of individual traits to a structure

-- |Adds the promise of commutativity to a structure.
addCommutativity :: GrouplikeStruct c a i u l r inv el -> GrouplikeStruct TagCommutative a i u l r inv el
addCommutativity (GrouplikeStruct _ a i u l r inv el) = GrouplikeStruct TagCommutative a i u l r inv el

-- |Adds the promise of associativity to a structure.
addAssociativity :: GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c TagAssociative i u l r inv el
addAssociativity (GrouplikeStruct c _ i u l r inv el) = GrouplikeStruct c TagAssociative i u l r inv el

-- |Adds the promise of idempotence to a structure.
addIdempotence :: GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c a TagIdempotent u l r inv el
addIdempotence (GrouplikeStruct c a _ u l r inv el) = GrouplikeStruct c a TagIdempotent u l r inv el

-- |Adds a unit element to a structure.
addUnitElement :: el -> GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c a i TagUnitElement l r inv el
addUnitElement ui (GrouplikeStruct c a i _ l r inv el) = GrouplikeStruct c a i (TagUnitElement ui) l r inv el

-- |Adds a left divider to a structure.
addLeftDivider :: Bin el -> GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c a i u TagLeftDivider r inv el
addLeftDivider ld (GrouplikeStruct c a i u _ r inv el) = GrouplikeStruct c a i u (TagLeftDivider ld) r inv el

-- |Adds a right divider to a structure.
addRightDivider :: Bin el -> GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c a i u l TagRightDivider inv el
addRightDivider rd (GrouplikeStruct c a i u l _ inv el) = GrouplikeStruct c a i u l (TagRightDivider rd) inv el

-- |Adds an invere function to a structure.
addInverse :: Un el -> GrouplikeStruct c a i u l r inv el -> GrouplikeStruct c a i u l r TagInverse el
addInverse i' (GrouplikeStruct c a i u l r _ el) = GrouplikeStruct c a i u l r (TagInverse i') el


-- Creation of structures

-- |Creates a magma.
makeMagma :: Bin el -- ^The structure's binary operation.
          -> MagmaStruct el
makeMagma = GrouplikeStruct TagUnknownCommutative TagUnknownAssociative TagUnknownIdempotent TagUnknownUnitElement TagUnknownLeftDivider TagUnknownRightDivider TagUnknownInverse

-- |Creates a quasigroup.
makeQuasigroup :: Bin el -- ^The structure's binary operation.
               -> Bin el -- ^Left divider.
               -> Bin el -- ^Right divider.
               -> QuasigroupStruct el
makeQuasigroup o ld rd = addRightDivider rd $ addLeftDivider ld $ makeMagma o

-- |Creates a semigroup.
makeSemigroup :: Bin el -- ^The structure's binary operation.
              -> SemigroupStruct el
makeSemigroup o = addAssociativity $ makeMagma o

-- |Creates a loop.
makeLoop :: Bin el -- ^The structure's binary operation.
         -> Bin el -- ^Left divider.
         -> Bin el -- ^Right divider.
         -> el     -- ^Unit element.
         -> LoopStruct el
makeLoop o ld rd ui = addUnitElement ui $ makeQuasigroup o ld rd

-- |Creates a monoid.
makeMonoid :: Bin el -- ^The structure's binary operation.
           -> el     -- ^Unit element.
           -> MonoidStruct el
makeMonoid o ui = addUnitElement ui $ makeSemigroup o

-- |Creates a commutative monoid.
makeCommutativeMonoid :: Bin el -- ^The structure's binary operation.
           -> el                 -- ^Unit element.
           -> CommutativeMonoidStruct el
makeCommutativeMonoid o ui = addCommutativity $ makeMonoid o ui

-- |Creates a group.
makeGroup :: Bin el -- ^The structure's binary operation (op).
          -> Un el  -- ^Inverse function inv. It also specifies the left and right dividers as: leftDivider a b = inv a `op` b; rightDivider a b = b `op` inv a
          -> el     -- ^Unit element.
          -> GroupStruct el
makeGroup o inv ui = addInverse inv $ addRightDivider rd $ addLeftDivider ld $ makeMonoid o ui
   where ld a b = inv a `o` b
         rd a b = b `o` inv a

-- |Creates a commutative group.
makeCommutativeGroup :: Bin el -- ^The structure's binary operation (op).
          -> Un el  -- ^Inverse function inv. It also specifies the left and right dividers as: leftDivider a b = inv a `op` b; rightDivider a b = b `op` inv a
          -> el     -- ^Unit element.
          -> CommutativeGroupStruct el
makeCommutativeGroup o inv ui = addCommutativity $ makeGroup o inv ui

-- |Creates a semilattice.
makeSemilattice :: Bin el -- ^The structure's binary operation.
                -> SemilatticeStruct el
makeSemilattice o = addIdempotence $ addCommutativity $ makeSemigroup o

-- |Creates a bounded semilattice.
makeBoundedSemilattice :: Bin el -- ^The structure's binary operation.
                       -> el     -- ^Unit element.
                       -> BoundedSemilatticeStruct el
makeBoundedSemilattice o ui = addUnitElement ui $ makeSemilattice o


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
            => GrouplikeStruct c a i u l r inv el -> DynamicGroupStruct el
makeDynamic g = (GrouplikeStruct (isCommutative g)
                                 (isAssociative g)
                                 (isIdempotent g)
                                 (getUnitElement g)
                                 (getLeftDivider g)
                                 (getRightDivider g)
                                 (getInverse g)
                                 (gOperation g))

-- Typesafe accessor functions for structures

-- |Returns whether the structure is commutative.
isCommutative :: (CommutativityTag c) => GrouplikeStruct c a i u l r inv el -> CommutativityValue
isCommutative = getCommutativityValue . gCommutativity

-- |Returns whether the structure is associative.
isAssociative :: (AssociativityTag a) => GrouplikeStruct c a i u l r inv el -> AssociativityValue
isAssociative = getAssociativityValue . gAssociativity

-- |Returns whether the structure is idempotent.
isIdempotent :: (IdempotenceTag i) => GrouplikeStruct c a i u l r inv el -> IdempotenceValue
isIdempotent = getIdempotenceValue . gIdempotence

-- |Returns the unit element of the structure, if it exists.
getUnitElement :: (UnitElementTag u) => GrouplikeStruct c a i u l r inv el -> UnitElementValue el
getUnitElement = getUnitElementValue . gUnitElement

-- |Returns the left divider function of the structure, if it exists.
getLeftDivider :: (LeftDividerTag l) => GrouplikeStruct c a i u l r inv el -> LeftDividerValue (Bin el)
getLeftDivider = getLeftDividerValue . gLeftDivider

-- |Returns the right divider function of the structure, if it exists.
getRightDivider :: (RightDividerTag r) => GrouplikeStruct c a i u l r inv el -> RightDividerValue (Bin el)
getRightDivider = getRightDividerValue . gRightDivider

-- |Returns the left divider and right divider functions of the structure, if they exists.
getDividers :: (LeftDividerTag l, RightDividerTag r) => GrouplikeStruct c a i u l r inv el -> (LeftDividerValue (Bin el), RightDividerValue (Bin el))
getDividers = liftA2 (,) getLeftDivider getRightDivider

-- |Returns the inverse function of the structure, if it exists.
getInverse :: (InverseTag inv) => GrouplikeStruct c a i u l r inv el -> InverseValue (Un el)
getInverse = getInverseValue . gInverse
