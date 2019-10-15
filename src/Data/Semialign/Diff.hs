{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

{-|

Module      : Data.Semialign.Diff
Description : Semialigns can be diffed, and sometimes patched
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : jack.kelly@data61.csiro.au
Stability   : experimental
Portability : Portable

The 'Semialign' typeclass lets us line up two structures of the same
type. It's then possible to take a simple diff by comparing the points
of overlap.

=== A note on type variables

The return type of the diffing functions is very general, because we
might want to (say) diff two @[a]@ into an @'Data.IntMap.IntMap' a@, a
@'Data.Map.Map' Int a@ or some other structure. This generality can
hurt type inference.

The type signatures for 'diff' and 'diffNoEq' have the return type as
their first type variable, so you can set the return type with a
single type application.

-}

module Data.Semialign.Diff
  ( diff
  , diffNoEq
  , patch
  ) where

import Control.Lens
  ( AsEmpty(..)
  , At(..)
  , pattern Empty
  , FoldableWithIndex(..)
  , Index
  , IxValue
  )
import Control.Lens.Operators
import Data.Semialign (Semialign(..))
import Data.These (These(..))

-- $setup
-- >>> :set -XTypeApplications
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
  :: forall p f a .
     ( FoldableWithIndex (Index p) f
     , Semialign f
     , Eq a
     , AsEmpty p
     , At p
     , IxValue p ~ (Maybe a)
     )
  => f a
  -> f a
  -> p
diff = (ifoldr step Empty .) . align
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
  :: forall p f a .
     ( FoldableWithIndex (Index p) f
     , Semialign f
     , AsEmpty p
     , At p
     , IxValue p ~ Maybe a
     )
  => f a
  -> f a
  -> p
diffNoEq = (ifoldr step Empty .) . align
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
-- @since 0.1.0.0
patch
  :: forall p m a .
     ( FoldableWithIndex (Index m) p
     , At m
     , IxValue m ~ a
     )
  => p (Maybe a)
  -> m
  -> m
patch p m = ifoldr step m p
  where
    step k Nothing = at k .~ Nothing
    step k (Just new) = at k ?~ new
