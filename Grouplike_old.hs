{-# LANGUAGE FlexibleInstances #-}

module Grouplike
  (UO,
   BO,
   Magma(..),
   Quasigroup(..),
   Loop(..),
   Semigroup,
   Semilattice,
   BoundedSemilattice,
   Monoid(..),
   CommutativeMonoid,
   Group(..),
   CommutativeGroup,
   MagmaStruct,
   QuasigroupStruct,
   LoopStruct,
   SemigroupStruct,
   SemilatticeStruct,
   BoundedSemilatticeStruct,
   MonoidStruct,
   CommutativeMonoidStruct,
   GroupStruct,
   CommutativeGroupStruct,
   makeMagma,
   makeQuasigroup,
   makeLoop,
   makeSemigroup,
   makeSemilattice,
   makeBoundedSemilattice,
   makeMonoid,
   makeCommutativeMonoid,
   makeGroup,
   makeCommutativeGroup)
where

-- |Shorthand for a binary operation.
type BO a = (a -> a -> a)
-- |Shorthand for a unary operation.
type UO a = (a -> a)


data MagmaTag = MagmaTag
data QuasigroupTag = QuasigroupTag
data LoopTag = LoopTag
data SemigroupTag = SemigroupTag
data SemilatticeTag = SemilatticeTag
data BoundedSemilatticeTag = BoundedSemilatticeTag
data MonoidTag = MonoidTag
data CommutativeMonoidTag = CommutativeMonoidTag
data GroupTag = GroupTag
data CommutativeGroupTag = CommutativeGroupTag

type MagmaStruct el kind = GrouplikeStruct MagmaTag el kind
type QuasigroupStruct el kind = GrouplikeStruct QuasigroupTag el kind
type LoopStruct el kind = GrouplikeStruct LoopTag el kind
type SemigroupStruct el kind = GrouplikeStruct SemigroupTag el kind
type SemilatticeStruct el kind = GrouplikeStruct SemilatticeTag el kind
type BoundedSemilatticeStruct el kind = GrouplikeStruct BoundedSemilatticeTag el kind
type MonoidStruct el kind = GrouplikeStruct MonoidTag el kind
type CommutativeMonoidStruct el kind = GrouplikeStruct CommutativeMonoidTag el kind
type GroupStruct el kind = GrouplikeStruct GroupTag el kind
type CommutativeGroupStruct el kind = GrouplikeStruct CommutativeGroupTag el kind

class GrouplikeTag a where

instance GrouplikeTag MagmaTag where
instance GrouplikeTag QuasigroupTag where
instance GrouplikeTag LoopTag where
instance GrouplikeTag SemigroupTag where
instance GrouplikeTag SemilatticeTag where
instance GrouplikeTag BoundedSemilatticeTag where
instance GrouplikeTag MonoidTag where
instance GrouplikeTag CommutativeMonoidTag where
instance GrouplikeTag GroupTag where
instance GrouplikeTag CommutativeGroupTag where


data GrouplikeStruct tag el kind = 
--  GrouplikeStruct :: GroupStructTag -> kind -> BO el -> BO el -> UO el -> el -> GrouplikeStruct GroupStructTag el kind
  GrouplikeStruct{gTag::tag,
                  gkind::kind,
                  gldiv::BO el,
                  grdiv::BO el,
                  gop::BO el,
                  ginv::UO el,
                  gident::el}

makeMagma :: BO el
          -> kind
          -> MagmaStruct el kind
makeMagma o k = GrouplikeStruct MagmaTag k undefined undefined o undefined undefined

makeQuasigroup :: BO el
               -> BO el
               -> BO el
               -> kind
               -> QuasigroupStruct el kind
makeQuasigroup o l r k = GrouplikeStruct QuasigroupTag k l r o undefined undefined

makeLoop :: BO el
            -> BO el
            -> BO el
            -> el
            -> kind
            -> LoopStruct el kind
makeLoop o l r i k = GrouplikeStruct LoopTag k l r o undefined i

makeSemigroup :: BO el
              -> kind
              -> SemigroupStruct el kind
makeSemigroup o k = GrouplikeStruct SemigroupTag k undefined undefined o undefined undefined

makeSemilattice :: BO el
                -> kind
                -> SemilatticeStruct el kind
makeSemilattice o k = GrouplikeStruct SemilatticeTag k undefined undefined o undefined undefined

makeBoundedSemilattice :: BO el
                       -> el
                       -> kind
                       -> BoundedSemilatticeStruct el kind
makeBoundedSemilattice o i k = GrouplikeStruct BoundedSemilatticeTag k undefined undefined o undefined i


makeMonoid :: BO el
           -> el
           -> kind
           -> MonoidStruct el kind
makeMonoid o i k = GrouplikeStruct MonoidTag k undefined undefined o undefined i

makeCommutativeMonoid :: BO el
                      -> el
                      -> kind
                      -> CommutativeMonoidStruct el kind
makeCommutativeMonoid o i k = GrouplikeStruct CommutativeMonoidTag k undefined undefined o undefined i

makeGroup :: BO el
          -> UO el
          -> el
          -> kind
          -> GroupStruct el kind
makeGroup o iv i k = GrouplikeStruct GroupTag k l r o iv i
   where r x y = iv x `o` y
         l x y = y `o` iv x

makeCommutativeGroup :: BO el
                     -> UO el
                     -> el
                     -> kind
                     -> CommutativeGroupStruct el kind
makeCommutativeGroup o iv i k = GrouplikeStruct CommutativeGroupTag k l r o iv i
   where r x y = iv x `o` y
         l x y = y `o` iv x

class Magma a where
  op :: a el k -> BO el
  kind :: a el k -> k

class Magma a => Quasigroup a where
  ldiv :: a el k -> BO el
  rdiv :: a el k -> BO el

class Quasigroup a => Loop a where
  ident :: a el k -> el

class Magma a => Semigroup a where

class Semigroup a => Monoid a where
  mident :: a el k -> el

class Semigroup a => Semilattice a where

class Semilattice a => BoundedSemilattice a where
  lident :: a el k -> el

class Monoid a => CommutativeMonoid a where

class (Monoid a, Loop a) => Group a where
  inv :: a el k -> UO el

class Group a => CommutativeGroup a where


instance Magma (GrouplikeStruct MagmaTag) where
  op = gop
  kind = gkind

instance Magma (GrouplikeStruct QuasigroupTag) where
  op = gop
  kind = gkind
instance Quasigroup (GrouplikeStruct QuasigroupTag) where
  ldiv = gldiv
  rdiv = grdiv

instance Magma (GrouplikeStruct LoopTag) where
  op = gop
  kind = gkind
instance Quasigroup (GrouplikeStruct LoopTag) where
  ldiv = gldiv
  rdiv = grdiv
instance Loop (GrouplikeStruct LoopTag) where
  ident = gident

instance Magma (GrouplikeStruct SemigroupTag) where
  op = gop
  kind = gkind
instance Semigroup (GrouplikeStruct SemigroupTag) where

instance Magma (GrouplikeStruct SemilatticeTag) where
  op = gop
  kind = gkind
instance Semigroup (GrouplikeStruct SemilatticeTag) where
instance Semilattice (GrouplikeStruct SemilatticeTag) where

instance Magma (GrouplikeStruct BoundedSemilatticeTag) where
  op = gop
  kind = gkind
instance Semigroup (GrouplikeStruct BoundedSemilatticeTag) where
instance Semilattice (GrouplikeStruct BoundedSemilatticeTag) where
instance BoundedSemilattice (GrouplikeStruct BoundedSemilatticeTag) where
  lident = gident

instance Magma (GrouplikeStruct MonoidTag) where
  op = gop
  kind = gkind
instance Semigroup (GrouplikeStruct MonoidTag) where
instance Monoid (GrouplikeStruct MonoidTag) where
  mident = gident

instance Magma (GrouplikeStruct CommutativeMonoidTag) where
  op = gop
  kind = gkind
instance Semigroup (GrouplikeStruct CommutativeMonoidTag) where
instance Monoid (GrouplikeStruct CommutativeMonoidTag) where
  mident = gident
instance CommutativeMonoid (GrouplikeStruct CommutativeMonoidTag) where

instance Magma (GrouplikeStruct GroupTag) where
  op = gop
  kind = gkind
instance Quasigroup (GrouplikeStruct GroupTag) where
  ldiv = gldiv
  rdiv = grdiv
instance Loop (GrouplikeStruct GroupTag) where
  ident = gident
instance Semigroup (GrouplikeStruct GroupTag) where
instance Monoid (GrouplikeStruct GroupTag) where
  mident = gident
instance Group (GrouplikeStruct GroupTag) where
  inv = ginv

instance Magma (GrouplikeStruct CommutativeGroupTag) where
  op = gop
  kind = gkind
instance Quasigroup (GrouplikeStruct CommutativeGroupTag) where
  ldiv = gldiv
  rdiv = grdiv
instance Loop (GrouplikeStruct CommutativeGroupTag) where
  ident = gident
instance Semigroup (GrouplikeStruct CommutativeGroupTag) where
instance Monoid (GrouplikeStruct CommutativeGroupTag) where
  mident = mident
instance CommutativeMonoid (GrouplikeStruct CommutativeGroupTag) where
instance Group (GrouplikeStruct CommutativeGroupTag) where
  inv = ginv
instance CommutativeGroup (GrouplikeStruct CommutativeGroupTag) where
