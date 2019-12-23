{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}

{-|

Module      : Data.Semialign.Diff
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
might want to (say) diff two @[a]@ into an @'Data.IntMap.IntMap' a@,
@'Data.Map.Map' Int a@ or some other structure. This generality can
hurt type inference.

The type signatures for all functions have the patch type as their
first type variable. For 'diff' \/ 'diffNoEq' \/ 'diffWith', this allows
setting the return type with a single type application.

-}

module Data.Semialign.Diff
  ( -- * Diffing
    diff
  , diffNoEq
  , diffWith
    -- * Patching
  , patch
  , patchWith
  ) where

import Control.Lens
  ( AsEmpty(..)
  , At(..)
  , pattern Empty
  , FoldableWithIndex(..)
  , Index
  , IxValue
  , set
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
  :: forall p f i a .
     ( FoldableWithIndex i f
     , Semialign f
     , Eq a
     , AsEmpty p
     , At p
     , Index p ~ i
     , IxValue p ~ (Maybe a)
     )
  => f a
  -> f a
  -> p
diff = diffWith $ \case
  This _ -> Just Nothing
  That new -> Just $ Just new
  These old new
    | old == new -> Nothing
    | otherwise -> Just $ Just new

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
  :: forall p f i a .
     ( FoldableWithIndex i f
     , Semialign f
     , AsEmpty p
     , At p
     , Index p ~ i
     , IxValue p ~ Maybe a
     )
  => f a
  -> f a
  -> p
diffNoEq = diffWith $ Just . \case
  This _ -> Nothing
  That new -> Just new
  These _ new -> Just new

-- | Diff two structures with a custom function.
--
-- This function should return 'Nothing' if there is no meaningful
-- change and @'Just' new@ to indicate a changed value.
--
-- Often, @c@ is itself a @'Maybe' d@, to indicate deletion/replacement
-- of a value.
--
-- @since 0.1.0.0
diffWith
  :: forall p f i a b c .
     ( FoldableWithIndex i f
     , Semialign f
     , AsEmpty p
     , At p
     , Index p ~ i
     , IxValue p ~ c
     )
  => (These a b -> Maybe c)
  -> f a
  -> f b
  -> p
diffWith f = (ifoldr step Empty .) . align
  where
    step k = set (at k) . f

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
  :: forall p m i a .
     ( FoldableWithIndex i p
     , At m
     , Index m ~ i
     , IxValue m ~ a
     )
  => p (Maybe a)
  -> m
  -> m
patch = patchWith $ const id

-- | Apply changes to a structure with a custom function, folding over
-- the patch.
--
-- The provided function receives two arguments: the old value if
-- present and the new value from the patch. It should return @'Just'
-- new@ to store @new@ into the result, or 'Nothing' to delete it.
--
-- @since 0.1.0.0
patchWith
  :: forall p m i a b .
     ( FoldableWithIndex i p
     , At m
     , Index m ~ i
     , IxValue m ~ a
     )
  => (Maybe a -> b -> Maybe a)
  -> p b
  -> m
  -> m
patchWith f p m = ifoldr step m p
  where
    step k v = at k %~ flip f v
