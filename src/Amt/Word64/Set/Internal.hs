{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Amt.Word64.Set.Internal
  ( Word64Set (..)
  , empty
  , singleton
  , null
  , size
  , member
  , notMember
  , insert
  , delete
  , alterF
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , filter
  , partition
  , map
  , mapMonotonic
  , foldr
  , foldl'
  , isSubsetOf
  , isProperSubsetOf
  , disjoint
  , fromList
  , toList
  , elems
  , valid
  , InvariantViolation (..)
  ) where

import Amt.Word64.Internal.Bits
  ( BitMatch (..)
  , Bitmap (..)
  , Index (..)
  , Shift
  , ShiftBox (..)
  , clearLowBit
  , index
  , indexMatch
  , lowBit
  , nextShift
  , shiftGE64
  , shiftToBox
  , shiftToInt
  )
import Control.DeepSeq (NFData (rnf))
import Control.Monad.ST (ST, runST)
import Data.Bits hiding (bit, shift)
import Data.Bits qualified as Bits
import Data.Data
  ( Constr
  , Data (..)
  , DataType
  , Fixity (Prefix)
  , mkConstr
  , mkDataType
  )
import Data.Foldable qualified as Foldable
import Data.Primitive.SmallArray
import Data.Word (Word64)
import GHC.Exts
  ( Word64#
  , build
  , eqWord64#
  , isTrue#
  , sameSmallArray#
  )
import GHC.Exts qualified as Exts
import GHC.Word (Word64 (W64#))
import Text.Read (Lexeme (Ident), lexP, parens, readPrec)
import Prelude hiding (filter, foldl', foldr, map, null)

data InvariantViolation
  = PrefixMismatch
      { ivKey :: !Word64
      , ivShift :: !ShiftBox
      , ivPrefix :: !Word64
      }
  | BitmapCountMismatch
      { ivBitmap :: !Word64
      , ivArraySize :: !Int
      }
  | RedundantBranch
      { ivPrefix :: !Word64
      }
  | UnexpectedEmptyBranch
  deriving (Show, Eq)

{- | An array-mapped trie with 64-bit word keys.

=== Invariants

1. __Canonical empty__: The empty set is represented by a 'Branch' with an
   empty bitmap.

2. __No redundant branches__: If a 'Branch' has exactly one sub-node, this
   node must be a 'Branch' node too.

3. __Bitmap consistency__: The number of set bits in the 'Bitmap' must
   exactly match the size of the 'SmallArray'.

4. __Prefix consistency__: For any node at 'Shift' @s@, all keys in its
   subtree must share the same prefix for the bits more significant than @s@.

5. __Empty only at root__: The canonical empty node may only appear at the
   root. Internal nodes are never empty.
-}
data Word64Set
  = Branch !Bitmap !(SmallArray Word64Set)
  | Leaf !Word64

instance Show Word64Set where
  show s = "fromList " ++ show (toList s)

instance Eq Word64Set where
  s1 == s2 = eqSet s1 s2

eqSet :: Word64Set -> Word64Set -> Bool
eqSet s1 s2 = eqSet_ s1 s2
 where
  eqSet_ (Leaf k1) (Leaf k2) = k1 == k2
  eqSet_ (Branch bm1 ary1) (Branch bm2 ary2) =
    bm1 == bm2 && eqSmallArray ary1 ary2
  eqSet_ _ _ = False

  eqSmallArray ary1 ary2
    | sameSmallArray ary1 ary2 = True
    | otherwise = loop (n - 1)
   where
    n = sizeofSmallArray ary1
    loop i
      | i < 0 = True
      | otherwise =
          let a1 = indexSmallArray ary1 i
              a2 = indexSmallArray ary2 i
           in eqSet a1 a2 && loop (i - 1)

sameSmallArray :: SmallArray a -> SmallArray a -> Bool
sameSmallArray (SmallArray a1#) (SmallArray a2#) =
  isTrue# (sameSmallArray# a1# a2#)
{-# INLINE sameSmallArray #-}

instance Ord Word64Set where
  compare s1 s2 = compare (toList s1) (toList s2)

instance Read Word64Set where
  readPrec = parens $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    pure (fromList xs)

instance Exts.IsList Word64Set where
  type Item Word64Set = Word64
  fromList = Amt.Word64.Set.Internal.fromList
  toList = Amt.Word64.Set.Internal.toList

instance Semigroup Word64Set where
  (<>) = union

instance Monoid Word64Set where
  mempty = empty
  mappend = (<>)

instance NFData Word64Set where
  rnf (Leaf _) = ()
  rnf (Branch _ ary) =
    Foldable.foldr (\s acc -> rnf s `seq` acc) () ary

instance Data Word64Set where
  gfoldl k z s = z fromList `k` toList s
  gunfold k z c
    | c == fromListConstr = k (z fromList)
    | otherwise = error "gunfold: expected fromList"
  toConstr _ = fromListConstr
  dataTypeOf _ = word64SetDataType

word64SetDataType :: DataType
word64SetDataType =
  mkDataType "Amt.Word64.Set.Word64Set" [fromListConstr]

fromListConstr :: Constr
fromListConstr = mkConstr word64SetDataType "fromList" [] Prefix

valid :: Word64Set -> Maybe InvariantViolation
valid (Branch (BM 0) ary)
  | n == 0 = Nothing
  | otherwise = Just $ BitmapCountMismatch 0 n
 where
  n = sizeofSmallArray ary
valid t = validInternal 0# 0 t

validInternal :: Shift -> Word64 -> Word64Set -> Maybe InvariantViolation
validInternal shift !prefix (Leaf k) =
  let mask =
        if shiftGE64 shift
          then complement 0
          else (Bits.bit (shiftToInt shift) :: Word64) - 1
   in if (k .&. mask) == prefix
        then Nothing
        else Just $ PrefixMismatch k (shiftToBox shift) prefix
validInternal _ _ (Branch (BM 0) _) = Just UnexpectedEmptyBranch
validInternal shift !prefix (Branch (BM bm) ary) =
  let n = sizeofSmallArray ary
   in if popCount bm /= n
        then Just $ BitmapCountMismatch bm n
        else validSubtrees shift prefix bm ary

validSubtrees ::
  Shift ->
  Word64 ->
  Word64 ->
  SmallArray Word64Set ->
  Maybe InvariantViolation
validSubtrees shift prefix bm ary
  | sizeofSmallArray ary == 1
  , Leaf{} <- indexSmallArray ary 0 =
      Just $ RedundantBranch prefix
  | otherwise = go
 where
  go =
    let children = Foldable.toList ary
        bits = [i | i <- [0 .. 63], testBit bm i]
     in Foldable.asum $
          zipWith
            ( \i child ->
                validInternal
                  (nextShift shift)
                  (prefix .|. (fromIntegral i `Bits.shiftL` shiftToInt shift :: Word64))
                  child
            )
            bits
            children

empty :: Word64Set
empty = Branch (BM 0) mempty
{-# NOINLINE empty #-}

singleton :: Word64 -> Word64Set
singleton !k = Leaf k

null :: Word64Set -> Bool
null (Branch (BM 0) _) = True
null _ = False

size :: Word64Set -> Int
size (Leaf _) = 1
size (Branch _ ary) = Foldable.sum (fmap size ary)

member :: Word64 -> Word64Set -> Bool
member !k m = case k of
  W64# ww -> memberAtShift# 0# ww m

memberAtShift :: Shift -> Word64 -> Word64Set -> Bool
memberAtShift shift !k = case k of
  W64# ww -> memberAtShift# shift ww

memberAtShift# :: Shift -> Word64# -> Word64Set -> Bool
memberAtShift# shift k = go shift
 where
  go _ (Leaf k') =
    case k' of
      W64# k'# ->
        case eqWord64# k k'# of
          1# -> True
          _ -> False
  go s (Branch (BM bm) ary) =
    case indexMatch s (W64# k) (BM bm) of
      Nothing -> False
      Just i -> go (nextShift s) (indexSmallArray ary i)

notMember :: Word64 -> Word64Set -> Bool
notMember !k m = not (member k m)

foldr :: (Word64 -> b -> b) -> b -> Word64Set -> b
foldr f z (Leaf k) = f k z
foldr f z (Branch _ ary) =
  Foldable.foldr (\s acc -> foldr f acc s) z ary

foldl' :: (b -> Word64 -> b) -> b -> Word64Set -> b
foldl' f z (Leaf k) = f z k
foldl' f z (Branch _ ary) =
  Foldable.foldl' (\acc s -> foldl' f acc s) z ary

insert :: Word64 -> Word64Set -> Word64Set
insert !k m = case m of
  Branch (BM 0) _ -> singleton k
  Leaf k'
    | k == k' -> m
    | otherwise -> two 0# k k'
  Branch (BM bm) ary ->
    case index 0# k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (nextShift 0#) k child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k) ary)

-- | Unsafe insert that mutates arrays in-place under the hood.
insertUnsafe :: Word64 -> Word64Set -> Word64Set
insertUnsafe !k m = case m of
  Branch (BM 0) _ -> singleton k
  _ -> runST (insertAtShiftUnsafe 0# k m)

-- | Only valid for internal nodes.
insertAtShift :: Shift -> Word64 -> Word64Set -> Word64Set
insertAtShift s !k m = case m of
  Leaf k'
    | k == k' -> m
    | otherwise -> two s k k'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertAtShift (nextShift s) k child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k) ary)

-- | Unsafe insert using in-place updates. Expects a non-empty root.
insertAtShiftUnsafe :: Shift -> Word64 -> Word64Set -> ST s Word64Set
insertAtShiftUnsafe s !k m = case m of
  Leaf k'
    | k == k' -> pure m
    | otherwise -> pure (two s k k')
  branch@(Branch (BM bm) ary) ->
    case index s k (BM bm) of
      Index _ i Match -> do
        let child = indexSmallArray ary i
        newChild <- insertAtShiftUnsafe (nextShift s) k child
        _ <- updateAtUnsafe i newChild ary
        pure branch
      Index (BM bit) i NoMatch ->
        pure (Branch (BM (bm .|. bit)) (insertAt i (Leaf k) ary))

-- | Only valid for internal nodes.
insertIfNotExistsAtShift :: Shift -> Word64 -> Word64Set -> Word64Set
insertIfNotExistsAtShift s !k m = case m of
  Leaf k'
    | k == k' -> m
    | otherwise -> two s k k'
  Branch (BM bm) ary ->
    case index s k (BM bm) of
      Index _ i Match ->
        let child = indexSmallArray ary i
            newChild = insertIfNotExistsAtShift (nextShift s) k child
         in Branch (BM bm) (updateAt i newChild ary)
      Index (BM bit) i NoMatch ->
        Branch (BM (bm .|. bit)) (insertAt i (Leaf k) ary)

two :: Shift -> Word64 -> Word64 -> Word64Set
two shift !k1 !k2 =
  let idx1 = fromIntegral ((k1 `Bits.shiftR` shiftToInt shift) .&. 0x3f)
      idx2 = fromIntegral ((k2 `Bits.shiftR` shiftToInt shift) .&. 0x3f)
   in if idx1 /= idx2
        then
          let bm = Bits.bit idx1 .|. Bits.bit idx2
              ary =
                if idx1 < idx2
                  then smallArrayFromList [Leaf k1, Leaf k2]
                  else smallArrayFromList [Leaf k2, Leaf k1]
           in Branch (BM bm) ary
        else
          let child = two (nextShift shift) k1 k2
              bm = Bits.bit idx1
           in Branch (BM bm) (smallArrayFromList [child])

delete :: Word64 -> Word64Set -> Word64Set
delete !k = deleteAtShift 0# k

deleteAtShift :: Shift -> Word64 -> Word64Set -> Word64Set
deleteAtShift shift !k m = go shift m
 where
  go _ (Leaf k') | k == k' = empty
  go _ leaf@(Leaf _) = leaf
  go s (Branch (BM bm) ary) =
    case index s k (BM bm) of
      Index _ _ NoMatch -> Branch (BM bm) ary
      Index (BM bit) i Match ->
        let child = indexSmallArray ary i
            newChild = go (nextShift s) child
         in if null newChild
              then
                let newBm = bm .&. complement bit
                    newAry = removeAt i ary
                 in collapse (BM newBm) newAry
              else
                let newAry = updateAt i newChild ary
                 in collapse (BM bm) newAry

alterF :: Functor f => (Bool -> f Bool) -> Word64 -> Word64Set -> f Word64Set
alterF f !k s =
  fmap
    (\b -> if b then insert k s else delete k s)
    (f (member k s))

union :: Word64Set -> Word64Set -> Word64Set
union m1 m2 = unionAtShiftHandleEmpty 0# m1 m2

unions :: [Word64Set] -> Word64Set
-- TODO: Compare with GHC.Data.Word64Set/IntSet and HashSet unions.
-- See https://github.com/haskell-unordered-containers/unordered-containers/issues/139.
unions = Foldable.foldl' union empty

unionAtShiftHandleEmpty :: Shift -> Word64Set -> Word64Set -> Word64Set
-- TODO: Consider inlining unionAtShiftNoEmpty here and validate Core/allocs.
unionAtShiftHandleEmpty shift m1 m2 = case (m1, m2) of
  (Branch (BM 0) _, _) -> m2
  (_, Branch (BM 0) _) -> m1
  _ -> unionAtShiftNoEmpty shift m1 m2

unionAtShiftNoEmpty :: Shift -> Word64Set -> Word64Set -> Word64Set
unionAtShiftNoEmpty shift m1 m2 = case (m1, m2) of
  (Leaf k1, _) -> insertAtShift shift k1 m2
  (_, Leaf k2) -> insertIfNotExistsAtShift shift k2 m1
  (Branch (BM bm1) ary1, Branch (BM bm2) ary2) ->
    let (newBm, newAry) =
          unionBranches
            bm1
            ary1
            bm2
            ary2
            (unionAtShiftNoEmpty (nextShift shift))
     in collapse (BM newBm) newAry

{- | Merge two branch arrays by walking the union bitmap once.

Assumes both bitmaps are non-zero (i.e. neither branch is empty), so the
union bitmap is also non-zero.

The @both@ function is used when a bit is present in both branches.
-}
unionBranches ::
  Word64 ->
  SmallArray Word64Set ->
  Word64 ->
  SmallArray Word64Set ->
  (Word64Set -> Word64Set -> Word64Set) ->
  (Word64, SmallArray Word64Set)
unionBranches bm1 ary1 bm2 ary2 both =
  let newBm = bm1 .|. bm2
      n = popCount newBm
   in ( newBm
      , runST $ do
          mary <- newSmallArray n empty
          let step !w !i1 !i2 !j
                | w == 0 = unsafeFreezeSmallArray mary
                | otherwise =
                    let bit = lowBit w
                        has1 = bm1 .&. bit /= 0
                        has2 = bm2 .&. bit /= 0
                        (child, i1', i2') = case (has1, has2) of
                          (True, True) ->
                            ( both
                                (indexSmallArray ary1 i1)
                                (indexSmallArray ary2 i2)
                            , i1 + 1
                            , i2 + 1
                            )
                          (True, False) ->
                            (indexSmallArray ary1 i1, i1 + 1, i2)
                          (False, True) ->
                            (indexSmallArray ary2 i2, i1, i2 + 1)
                          (False, False) -> error "unionBranches: impossible"
                        w' = clearLowBit w
                     in do
                          writeSmallArray mary j child
                          step w' i1' i2' (j + 1)
          step newBm 0 0 0
      )

infixl 9 \\

(\\) :: Word64Set -> Word64Set -> Word64Set
(\\) = difference

difference :: Word64Set -> Word64Set -> Word64Set
difference m1_ m2_ = go 0# m1_ m2_
 where
  go _ (Branch (BM 0) _) _ = empty
  go _ m1 (Branch (BM 0) _) = m1
  go shift (Leaf k1) m2 =
    if memberAtShift shift k1 m2
      then empty
      else Leaf k1
  go shift m1 (Leaf k2) =
    if memberAtShift shift k2 m1
      then deleteAtShift shift k2 m1
      else m1
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let (newBm, newAry) = runST (differenceBranches shift bm1 ary1 bm2 ary2)
     in collapse (BM newBm) newAry

  differenceBranches ::
    forall s.
    Shift ->
    Word64 ->
    SmallArray Word64Set ->
    Word64 ->
    SmallArray Word64Set ->
    ST s (Word64, SmallArray Word64Set)
  differenceBranches shift bm1 ary1 bm2 ary2 = do
    let unionBm = bm1 .|. bm2
        n = popCount bm1
    if n == 0
      then pure (0, mempty)
      else do
        mary <- newSmallArray n empty
        let finish j bmAcc =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (bmAcc, newAry)
            step !w !i1 !i2 !j !newBm
              | w == 0 = finish j newBm
              | otherwise =
                  let bit = lowBit w
                      has1 = bm1 .&. bit /= 0
                      has2 = bm2 .&. bit /= 0
                      (m1, i1') =
                        if has1
                          then (Just (indexSmallArray ary1 i1), i1 + 1)
                          else (Nothing, i1)
                      (m2, i2') =
                        if has2
                          then (Just (indexSmallArray ary2 i2), i2 + 1)
                          else (Nothing, i2)
                      w' = clearLowBit w
                   in case (m1, m2) of
                        (Just c1, Just c2) ->
                          let child = go (nextShift shift) c1 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        (Just c1, Nothing) -> do
                          writeSmallArray mary j c1
                          step w' i1' i2' (j + 1) (newBm .|. bit)
                        _ -> step w' i1' i2' j newBm
        step unionBm 0 0 0 0

intersection :: Word64Set -> Word64Set -> Word64Set
intersection m1_ m2_ = go 0# m1_ m2_
 where
  go _ (Branch (BM 0) _) _ = empty
  go _ _ (Branch (BM 0) _) = empty
  go shift (Leaf k1) m2 =
    if memberAtShift shift k1 m2
      then Leaf k1
      else empty
  go shift m1 (Leaf k2) =
    if memberAtShift shift k2 m1
      then Leaf k2
      else empty
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    let (newBm, newAry) = runST (intersectionBranches shift bm1 ary1 bm2 ary2)
     in collapse (BM newBm) newAry

  intersectionBranches ::
    forall s.
    Shift ->
    Word64 ->
    SmallArray Word64Set ->
    Word64 ->
    SmallArray Word64Set ->
    ST s (Word64, SmallArray Word64Set)
  intersectionBranches shift bm1 ary1 bm2 ary2 = do
    let commonBm = bm1 .&. bm2
        unionBm = bm1 .|. bm2
        n = popCount commonBm
    if n == 0
      then pure (0, mempty)
      else do
        mary <- newSmallArray n empty
        let finish j bmAcc =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (bmAcc, newAry)
            step !w !i1 !i2 !j !newBm
              | w == 0 = finish j newBm
              | otherwise =
                  let bit = lowBit w
                      has1 = bm1 .&. bit /= 0
                      has2 = bm2 .&. bit /= 0
                      (m1, i1') =
                        if has1
                          then (Just (indexSmallArray ary1 i1), i1 + 1)
                          else (Nothing, i1)
                      (m2, i2') =
                        if has2
                          then (Just (indexSmallArray ary2 i2), i2 + 1)
                          else (Nothing, i2)
                      w' = clearLowBit w
                   in case (m1, m2) of
                        (Just c1, Just c2) ->
                          let child = go (nextShift shift) c1 c2
                           in if null child
                                then step w' i1' i2' j newBm
                                else do
                                  writeSmallArray mary j child
                                  step w' i1' i2' (j + 1) (newBm .|. bit)
                        _ -> step w' i1' i2' j newBm
        step unionBm 0 0 0 0

filter :: (Word64 -> Bool) -> Word64Set -> Word64Set
filter f = go
 where
  go (Leaf k) = if f k then Leaf k else empty
  go (Branch (BM bm) ary)
    -- TODO: This check is only needed for the root; consider hoisting it.
    | bm == 0 = empty
    | otherwise =
        let (newBm, newAry) = runST (goArray bm ary)
         in collapse (BM newBm) newAry

  goArray ::
    forall s.
    Word64 ->
    SmallArray Word64Set ->
    ST s (Word64, SmallArray Word64Set)
  goArray bm ary = do
    let n = sizeofSmallArray ary
    mary <- newSmallArray n empty
    let step !w !i !j !newBm
          | w == 0 =
              if j == 0
                then pure (0, mempty)
                else do
                  shrinkSmallMutableArray mary j
                  newAry <- unsafeFreezeSmallArray mary
                  pure (newBm, newAry)
          | otherwise =
              let bit = lowBit w
                  child = go (indexSmallArray ary i)
                  w' = clearLowBit w
               in if null child
                    then step w' (i + 1) j newBm
                    else do
                      writeSmallArray mary j child
                      step w' (i + 1) (j + 1) (newBm .|. bit)
    step bm 0 0 0

partition :: (Word64 -> Bool) -> Word64Set -> (Word64Set, Word64Set)
partition f m = go m
 where
  go (Leaf k)
    | f k = (Leaf k, empty)
    | otherwise = (empty, Leaf k)
  go (Branch (BM 0) _) = (empty, empty)
  go (Branch (BM bm) ary) =
    let (lBm, lAry, rBm, rAry) = partitionBranch bm ary
        l = collapse (BM lBm) lAry
        r = collapse (BM rBm) rAry
     in (l, r)

  partitionBranch ::
    Word64 ->
    SmallArray Word64Set ->
    (Word64, SmallArray Word64Set, Word64, SmallArray Word64Set)
  partitionBranch bm ary = runST (goArray bm ary)

  goArray ::
    forall s.
    Word64 ->
    SmallArray Word64Set ->
    ST s (Word64, SmallArray Word64Set, Word64, SmallArray Word64Set)
  goArray bm ary = do
    let n = sizeofSmallArray ary
    maryL <- newSmallArray n empty
    maryR <- newSmallArray n empty
    let finish ::
          SmallMutableArray s Word64Set ->
          Int ->
          Word64 ->
          ST s (Word64, SmallArray Word64Set)
        finish mary j bmAcc =
          if j == 0
            then pure (0, mempty)
            else do
              shrinkSmallMutableArray mary j
              newAry <- unsafeFreezeSmallArray mary
              pure (bmAcc, newAry)
        step !w !i !jl !jr !lBm !rBm
          | w == 0 = do
              (lBm', lAry) <- finish maryL jl lBm
              (rBm', rAry) <- finish maryR jr rBm
              pure (lBm', lAry, rBm', rAry)
          | otherwise =
              let bit = lowBit w
                  child = indexSmallArray ary i
                  (lChild, rChild) = go child
                  w' = clearLowBit w
                  (jl', lBm') =
                    if null lChild
                      then (jl, lBm)
                      else (jl + 1, lBm .|. bit)
                  (jr', rBm') =
                    if null rChild
                      then (jr, rBm)
                      else (jr + 1, rBm .|. bit)
               in do
                    if null lChild
                      then pure ()
                      else writeSmallArray maryL jl lChild
                    if null rChild
                      then pure ()
                      else writeSmallArray maryR jr rChild
                    step w' (i + 1) jl' jr' lBm' rBm'
    step bm 0 0 0 0 0

map :: (Word64 -> Word64) -> Word64Set -> Word64Set
map f = map_ empty
 where
  map_ !acc (Leaf k) = insertUnsafe (f k) acc
  map_ !acc (Branch _ ary) = Foldable.foldl' map_ acc ary

{- | Compatibility with GHC's Word64Set. Monotonicity does not make this
faster than 'map' in this implementation.
-}
mapMonotonic :: (Word64 -> Word64) -> Word64Set -> Word64Set
mapMonotonic = map

isSubsetOf :: Word64Set -> Word64Set -> Bool
-- TODO: Revisit isSubsetOf performance (and the Map version) once we
-- have Core/bench numbers.
isSubsetOf s1 s2 = go 0# s1 s2
 where
  go _ (Branch (BM 0) _) _ = True
  go _ _ (Branch (BM 0) _) = False
  go shift (Leaf k1) s = memberAtShift shift k1 s
  go _ (Branch _ _) (Leaf _) = False
  go shift (Branch (BM bm1) ary1) (Branch (BM bm2) ary2) =
    submapBranch (nextShift shift) bm1 ary1 bm2 ary2

  submapBranch ::
    Shift ->
    Word64 ->
    SmallArray Word64Set ->
    Word64 ->
    SmallArray Word64Set ->
    Bool
  submapBranch shift bm1 ary1 bm2 ary2 = step (bm1 .|. bm2) 0 0
   where
    step !w !i1 !i2
      | w == 0 = True
      | otherwise =
          let bit = lowBit w
              has1 = bm1 .&. bit /= 0
              has2 = bm2 .&. bit /= 0
              w' = clearLowBit w
           in case (has1, has2) of
                (True, True) ->
                  let c1 = indexSmallArray ary1 i1
                      c2 = indexSmallArray ary2 i2
                   in if go shift c1 c2
                        then step w' (i1 + 1) (i2 + 1)
                        else False
                (True, False) -> False
                (False, True) -> step w' i1 (i2 + 1)
                (False, False) -> step w' i1 i2

isProperSubsetOf :: Word64Set -> Word64Set -> Bool
isProperSubsetOf s1 s2 = isSubsetOf s1 s2 && size s1 < size s2

disjoint :: Word64Set -> Word64Set -> Bool
disjoint s1 s2 = null (intersection s1 s2)

fromList :: [Word64] -> Word64Set
fromList = Foldable.foldl' (\m !k -> insertUnsafe k m) empty

toList :: Word64Set -> [Word64]
toList = \s -> build (\c n -> foldr c n s)
{-# INLINE toList #-}

elems :: Word64Set -> [Word64]
elems = toList

insertAt :: Int -> a -> SmallArray a -> SmallArray a
insertAt i a ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray (n + 1) a
  copySmallArray mary 0 ary 0 i
  copySmallArray mary (i + 1) ary i (n - i)
  pure mary

updateAt :: Int -> a -> SmallArray a -> SmallArray a
updateAt i a ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray n a
  copySmallArray mary 0 ary 0 n
  writeSmallArray mary i a
  pure mary

updateAtUnsafe :: Int -> a -> SmallArray a -> ST s (SmallArray a)
updateAtUnsafe i a ary = do
  mary <- unsafeThawSmallArray ary
  writeSmallArray mary i a
  unsafeFreezeSmallArray mary

removeAt :: Int -> SmallArray a -> SmallArray a
removeAt i ary = runSmallArray $ do
  let n = sizeofSmallArray ary
  mary <- newSmallArray (n - 1) (error "removeAt")
  copySmallArray mary 0 ary 0 i
  copySmallArray mary i ary (i + 1) (n - i - 1)
  pure mary

collapse :: Bitmap -> SmallArray Word64Set -> Word64Set
collapse bm ary = case sizeofSmallArray ary of
  0 -> empty
  1 -> case indexSmallArray ary 0 of
    l@Leaf{} -> l
    _ -> Branch bm ary
  _ -> Branch bm ary
