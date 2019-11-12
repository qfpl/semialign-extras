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

merge
  :: Semialign t
  => (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> t a
  -> t b
  -> t c
merge f g h = alignWith (these f g h)

mergeMaybe
  :: (Filterable t, Semialign t)
  => (a -> Maybe c)
  -> (b -> Maybe c)
  -> (a -> b -> Maybe c)
  -> t a
  -> t b
  -> t c
mergeMaybe f g h = (catMaybes .) . alignWith (these f g h)

mergeA
  :: (Applicative f, Semialign t, Traversable t)
  => (a -> f c)
  -> (b -> f c)
  -> (a -> b -> f c)
  -> t a
  -> t b
  -> f (t c)
mergeA f g h = (sequenceA .) . alignWith (these f g h)

mergeMaybeA
  :: (Applicative f, Semialign t, Witherable t)
  => (a -> f (Maybe c))
  -> (b -> f (Maybe c))
  -> (a -> b -> f (Maybe c))
  -> t a
  -> t b
  -> f (t c)
mergeMaybeA f g h = (wither id .) . alignWith (these f g h)

imerge
  :: (SemialignWithIndex i t)
  => (i -> a -> c)
  -> (i -> b -> c)
  -> (i -> a -> b -> c)
  -> t a
  -> t b
  -> t c
imerge f g h = ialignWith (these <$> f <*> g <*> h)

imergeMaybe
  :: (Filterable t, SemialignWithIndex i t)
  => (i -> a -> Maybe c)
  -> (i -> b -> Maybe c)
  -> (i -> a -> b -> Maybe c)
  -> t a
  -> t b
  -> t c
imergeMaybe f g h = (catMaybes .) . ialignWith (these <$> f <*> g <*> h)

imergeA
  :: (Applicative f, SemialignWithIndex i t, Traversable t)
  => (i -> a -> f c)
  -> (i -> b -> f c)
  -> (i -> a -> b -> f c)
  -> t a
  -> t b
  -> f (t c)
imergeA f g h = (sequenceA .) . ialignWith (these <$> f <*> g <*> h)

imergeMaybeA
  :: (Applicative f, SemialignWithIndex i t, Witherable t)
  => (i -> a -> f (Maybe c))
  -> (i -> b -> f (Maybe c))
  -> (i -> a -> b -> f (Maybe c))
  -> t a
  -> t b
  -> f (t c)
imergeMaybeA f g h = (wither id .) . ialignWith (these <$> f <*> g <*> h)