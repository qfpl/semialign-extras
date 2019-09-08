{-# LANGUAGE LambdaCase #-}

module Data.Semialign.Diff
  ( -- * Diffing/patching
    diff
  , diffNoEq
  , patch

    -- * Indexed Variants
  , idiff
  , idiffNoEq

    -- * Helpers
  , alignWithMaybe
  , ialignWithMaybe
  ) where

import Data.Semialign (Semialign(..))
import Data.Semialign.Indexed (SemialignWithIndex(..))
import Data.These (These(..))
import Data.Witherable (Filterable(..))

-- $setup
-- >>> import Data.Map (Map, (!), fromList)

-- | Diff two structures. This is most useful if your @f@ is
-- explicitly-indexed ('Map'-like):
--
-- >>> diff (fromList [("Alice", 1), ("Bob", 2)]) (fromList [("Alice", 3), ("Carol", 4)])
-- fromList [("Alice",Just 3),("Bob",Nothing),("Carol",Just 4)]
--
-- If your structure is indexed, but the index is implicit in the
-- structure (e.g., lists), this probably won't do what you
-- expect. Consider 'idiff' instead:
--
-- >>> diff [1, 2, 3, 4, 5] [1, 3, 3, 4]
-- [Just 3,Nothing]
--
-- @since 0.1.0.0
diff :: (Filterable f, Semialign f, Eq a) => f a -> f a -> f (Maybe a)
diff = alignWithMaybe $ \case
  This{} -> Just Nothing
  That new -> Just $ Just new
  These old new
    | old == new -> Nothing
    | otherwise -> Just $ Just new

-- | Diff two structures without requiring an 'Eq' instance. Instead,
-- always assume a new value wherever the structures align:
--
-- >>> ($ 3) <$> diffNoEq (fromList [(1, (+ 1))]) (fromList [(1, (* 2))]) ! 1
-- Just 6
--
-- The same caveats for implicitly-indexed structures apply here just as
-- they did for 'diff'. Consider 'Data.Semialign.Indexed.idiffNoEq'
-- instead.
--
-- @since 0.1.0.0
diffNoEq :: (Filterable f, Semialign f) => f a -> f a -> f (Maybe a)
diffNoEq = alignWithMaybe $ Just . \case
  This{} -> Nothing
  That new -> Just new
  These _ new -> Just new

-- | Apply a patch to a structure.
--
-- >>> patch (fromList [(0, Just 0), (1, Just 3), (2, Nothing)]) (fromList [(0, 1), (2, 3)])
-- fromList [(0,0),(1,3)]
--
-- For explicitly-indexed structures, 'patch' undoes 'diff' / 'diffNoEq':
--
-- prop> \old new -> let typeOld = old :: Map Int Int in (patch (diff old new) old) == new
-- prop> \old new -> let typeOld = old :: Map Int Int in (patch (diffNoEq old new) old) == new
--
-- @since 0.1.0.0
patch :: (Semialign f, Filterable f) => f (Maybe a) -> f a -> f a
patch = alignWithMaybe $ \case
  This mnew -> mnew
  That old -> Just old
  These mnew _ -> mnew

-- | Diff two indexed structures:
--
-- >>> idiff [1, 2, 3, 4, 5] [1, 3, 3, 7]
-- [(1,Just 3),(3,Just 7),(4,Nothing)]
--
-- If your structure is explicitly indexed ('Map'-like), the result
-- type is awkward, and you may prefer 'diff' instead:
--
-- >>> idiff (fromList [(0, 1), (2, 3)]) (fromList [(0, 0), (1, 2)])
-- fromList [(0,(0,Just 0)),(1,(1,Just 2)),(2,(2,Nothing))]
--
-- @since 0.1.0.0
idiff
  :: (SemialignWithIndex i f, Filterable f, Eq a)
  => f a -> f a -> f (i, Maybe a)
idiff = ialignWithMaybe merge where
  merge i (This _) = Just (i, Nothing)
  merge i (That new) = Just (i, Just new)
  merge i (These old new)
    | old == new = Nothing
    | otherwise = Just (i, Just new)

-- | Diff two indexed structures without requiring an 'Eq'
-- instance. Instead, assume a new value wherever the structures
-- align:
--
-- >>> (fmap . fmap) ($ 3) (idiffNoEq [(+1)] [(*2), (const 0)] !! 1)
-- (1,Just 0)
--
-- @since 0.1.0.0
idiffNoEq
  :: (SemialignWithIndex i f, Filterable f)
  => f a -> f a -> f (i, Maybe a)
idiffNoEq = ialignWithMaybe merge where
  merge i (This _) = Just (i, Nothing)
  merge i (That new) = Just (i, Just new)
  merge i (These _ new) = Just (i, Just new)

-- | 'alignWith', then 'catMaybes'.
--
-- @since 0.1.0.0
alignWithMaybe
  :: (Filterable f, Semialign f)
  => (These a b -> Maybe c)
  -> f a
  -> f b
  -> f c
alignWithMaybe f fa fb = catMaybes $ alignWith f fa fb

-- | 'ialignWith', then 'catMaybes'.
--
-- @since 0.1.0.0
ialignWithMaybe
  :: (Filterable f, SemialignWithIndex i f)
  => (i -> These a b -> Maybe c)
  -> f a
  -> f b
  -> f c
ialignWithMaybe f fa fb = catMaybes $ ialignWith f fa fb
