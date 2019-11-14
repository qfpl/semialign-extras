{-|

Module      : Data.Semialign.Merge
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : jack.kelly@data61.csiro.au
Stability   : experimental
Portability : Portable

The 'Semialign' typeclass lets us line up two structures of the same
type. We can use this to merge two structures, or add additional
typeclasses to do filtering, 'Applicative' effects, or tracking
indices.

'merge' is the simplest function. It takes five arguments: functions
to handle values present in the left \/ right \/ both structures and
two structures to merge.

@
merge :: Semialign t
  => (a -> c) -> (b -> c) -> (a -> b -> c)
  -> t a -> t b
  -> t c
@

Every other function in this module is a variant with modified
functionality based on its name:

* Prefix @i@ means an "indexed" variant: each function argument takes
  an additional index (@i@) parameter.

* Suffix @Maybe@ means "filter results": each function argument
  returns @Maybe c@, and @Nothing@s are filtered out.

* Suffix @A@ means "applicative": each function argument returns
  actions in some 'Applicative' @f@, and the whole operation collects
  these actions to produce the a merged structure in @f@.

-}

module Data.Semialign.Merge
  ( merge
  , mergeMaybe
  , mergeA
  , mergeMaybeA

  -- * Indexed Variants
  , imerge
  , imergeMaybe
  , imergeA
  , imergeMaybeA
  ) where

import Data.Semialign (Semialign(..))
import Data.Semialign.Indexed (SemialignWithIndex(..))
import Data.These (these)
import Data.Witherable (Filterable(..), Witherable(..))

-- | @since 0.1.0.0
merge
  :: Semialign t
  => (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> t a
  -> t b
  -> t c
merge f g h = alignWith (these f g h)

-- | @since 0.1.0.0
mergeMaybe
  :: (Filterable t, Semialign t)
  => (a -> Maybe c)
  -> (b -> Maybe c)
  -> (a -> b -> Maybe c)
  -> t a
  -> t b
  -> t c
mergeMaybe f g h = (catMaybes .) . alignWith (these f g h)

-- | @since 0.1.0.0
mergeA
  :: (Applicative f, Semialign t, Traversable t)
  => (a -> f c)
  -> (b -> f c)
  -> (a -> b -> f c)
  -> t a
  -> t b
  -> f (t c)
mergeA f g h = (sequenceA .) . alignWith (these f g h)

-- | @since 0.1.0.0
mergeMaybeA
  :: (Applicative f, Semialign t, Witherable t)
  => (a -> f (Maybe c))
  -> (b -> f (Maybe c))
  -> (a -> b -> f (Maybe c))
  -> t a
  -> t b
  -> f (t c)
mergeMaybeA f g h = (wither id .) . alignWith (these f g h)

-- | @since 0.1.0.0
imerge
  :: (SemialignWithIndex i t)
  => (i -> a -> c)
  -> (i -> b -> c)
  -> (i -> a -> b -> c)
  -> t a
  -> t b
  -> t c
imerge f g h = ialignWith (these <$> f <*> g <*> h)

-- | @since 0.1.0.0
imergeMaybe
  :: (Filterable t, SemialignWithIndex i t)
  => (i -> a -> Maybe c)
  -> (i -> b -> Maybe c)
  -> (i -> a -> b -> Maybe c)
  -> t a
  -> t b
  -> t c
imergeMaybe f g h = (catMaybes .) . ialignWith (these <$> f <*> g <*> h)

-- | @since 0.1.0.0
imergeA
  :: (Applicative f, SemialignWithIndex i t, Traversable t)
  => (i -> a -> f c)
  -> (i -> b -> f c)
  -> (i -> a -> b -> f c)
  -> t a
  -> t b
  -> f (t c)
imergeA f g h = (sequenceA .) . ialignWith (these <$> f <*> g <*> h)

-- | @since 0.1.0.0
imergeMaybeA
  :: (Applicative f, SemialignWithIndex i t, Witherable t)
  => (i -> a -> f (Maybe c))
  -> (i -> b -> f (Maybe c))
  -> (i -> a -> b -> f (Maybe c))
  -> t a
  -> t b
  -> f (t c)
imergeMaybeA f g h = (wither id .) . ialignWith (these <$> f <*> g <*> h)
