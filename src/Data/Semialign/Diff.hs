{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Semialign.Diff
  ( diff
  , diffNoEq
  , patch
  ) where

import Control.Lens (At(..), FoldableWithIndex(..), Index, IxValue)
import Control.Lens.Operators
import Data.Semialign (Semialign(..))
import Data.These (These(..))

-- $setup
-- >>> :set -XGeneralizedNewtypeDeriving -XStandaloneDeriving -XTypeApplications
-- >>> import Data.Map (Map, (!), fromList)

-- | Diff two structures.
--
-- >>> :{
--   let
--     old = fromList [("Alice", 1), ("Bob", 2)]
--     new = fromList [("Alice", 3), ("Carol", 4)]
--   in
--     diff old new :: Map String (Maybe Int)
-- :}
-- fromList [("Alice",Just 3),("Bob",Nothing),("Carol",Just 4)]
--
-- @since 0.1.0.0
diff
  :: forall p f a i .
     ( FoldableWithIndex i f
     , Semialign f
     , Eq a
     , Monoid p
     , At p
     , Index p ~ i
     , IxValue p ~ (Maybe a)
     )
  => f a
  -> f a
  -> p
diff = (ifoldr step mempty .) . align
  where
    step k (This _) = at k ?~ Nothing
    step k (That new) = at k ?~ Just new
    step k (These old new)
      | new == old = id
      | otherwise = at k ?~ Just new

-- | Diff two structures without requiring an 'Eq' instance. Instead,
-- always assume a new value wherever the structures align:
--
-- >>> :{
--   let
--     old = fromList [("Alice", (+ 1))]
--     new = fromList [("Alice", (* 2))]
--   in
--     ($ 3) <$> diffNoEq old new ! "Alice"
-- :}
-- Just 6
--
-- @since 0.1.0.0
diffNoEq
  :: forall p f a i .
     ( FoldableWithIndex i f
     , Semialign f
     , Monoid p
     , At p
     , Index p ~ i
     , IxValue p ~ Maybe a
     )
  => f a
  -> f a
  -> p
diffNoEq = (ifoldr step mempty .) . align
  where
    step k (This _) = at k ?~ Nothing
    step k (That new) = at k ?~ Just new
    step k (These _ new) = at k ?~ Just new

-- | Apply a patch to a structure.
--
-- >>> patch (fromList [(0, Just 0), (1, Just 3), (2, Nothing)]) (fromList [(0, 1), (2, 3)])
-- fromList [(0,0),(1,3)]
--
-- When the types are compatible, 'patch' undoes 'diff' / 'diffNoEq':
--
-- prop> \old new -> let p = diff @(Map Int (Maybe Int)) old (new :: Map Int Int) in (patch p old) == new
-- prop> \old new -> let p = diffNoEq @(Map Int (Maybe Int)) old (new :: Map Int Int) in (patch p old) == new
--
-- If @p@ is a 'Map', 'patch' is a
-- <https://en.wikipedia.org/wiki/Semigroup_action monoid action> on
-- 'm':
--
-- prop> \p1 p2 m -> let p1Type = p1 :: Map Int (Maybe Int) in patch (p1 <> p2) (m :: Map Int Int) == patch p1 (patch p2 m)
-- prop> \m -> let nil = mempty :: Map Int (Maybe Int) in patch nil (m :: Map Int Int) == m
--
-- This is not true for every monoidal @p@.
--
-- @since 0.1.0.0
patch
  :: forall p i m a .
     ( FoldableWithIndex i p
     , At m
     , Index m ~ i
     , IxValue m ~ a
     )
  => p (Maybe a)
  -> m
  -> m
patch = flip $ ifoldr step
  where
    step k ma = at k .~ ma
