{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Semialign.Merge.Indexed where
import           Data.Bool (bool)
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map.Merge.Lazy as Map
import           Data.Semialign.Indexed (SemialignWithIndex(..))
import           Data.Witherable (WitherableWithIndex(..))

-- | Class for indexed functors which support an efficient filtering
-- merge. It is possible to construct a filtering merge using only
-- 'Semialign' and 'Witherable':
--
-- @
-- isimpleMergeA
--   :: ('SemialignWithIndex' i t, 'WitherableWithIndex' i t, 'Applicative' f)
--   => (i -> a -> f ('Maybe' c))
--   -> (i -> b -> f ('Maybe' c))
--   -> (i -> a -> b -> f ('Maybe' c))
--   -> t a
--   -> t b
--   -> f (t c)
-- isimpleMergeA left right match ta tb
--   = 'iwither' (const id) $ 'ialignWith' f ta tb
--
--   where
--     f i ('Data.These.This a') = left i a
--     f i ('Data.These.That b') = right i b
--     f i ('Data.These.These a b') = match i a b
-- @
--
-- Instances of 'MergeWithIndex t' provide an optimised 'imergeA' for
-- which the following should hold for any functions @left@, @right@
-- and @match@:
--
-- @
-- isimpleMergeA left right match = 'imergeA'
--   ('itraverseMaybeMissing' left)
--   ('itraverseMaybeMissing' right)
--   ('izipWithMaybeAMatched' match)
-- @
--
-- Similarly, for any tactics @leftT@, @rightT@ and @matchT@:
--
-- @
-- 'imergeA' leftT rightT matchT = isimpleMergeA
--   ('irunWhenMissing' leftT)
--   ('irunWhenMissing' rightT)
--   ('irunWhenMatched' matchT)
-- @
--
-- The associated data families 'IWhenMissing' and 'IWhenMatched' are
-- abstract, type-specific representations of the functions used to
-- handle values present in either or both sides of a merge. Often
-- these wrap multiple functions specialised to the structure of @t@,
-- so that large subsections of @t@ can be handled efficiently.
class
  (SemialignWithIndex i t, WitherableWithIndex i t)
  => MergeWithIndex i t | t -> i where

  -- | Merge two structures with an 'Applicative' action.
  imergeA
    :: Applicative f
    => IWhenMissing t f i a c
    -- ^ Handle values present on the left but not the right
    -> IWhenMissing t f i b c
    -- ^ Handle values present on the right but not the left
    -> IWhenMatched t f i a b c
    -- ^ Handle values present in both instances
    -> t a
    -> t b
    -> f (t c)

  -- | An abstract representation of a function @i -> a -> f (Maybe
  -- c)@, for when a value is present in only one side of a merge.
  data IWhenMissing t (f :: Type -> Type) i a c :: Type

  -- | Witness the isomorphism between functions and
  -- 'IWithMissing'. (The other half is 'traverseMaybeMissing'.)
  irunWhenMissing :: IWhenMissing t f i a c -> i -> a -> f (Maybe c)

  -- | An abstract representation of a function @i -> a -> b -> f
  -- (Maybe c)@, for when a value is present in both sides of a merge.
  data IWhenMatched t (f :: Type -> Type) i a b c :: Type

  -- | Withness the isomorphism between functions and
  -- 'IWithMatched'. (The other half is 'zipWithMaybeAMatched'.)
  irunWhenMatched :: IWhenMatched t f i a b c -> i -> a -> b -> f (Maybe c)

  -- | The fundamental way to construct a 'IWhenMissing'.
  --
  -- Other functions that return 'IWhenMissing' are specified as class
  -- methods so instances can replace them with optimised versions.
  iwitherMissing
    :: Applicative f
    => (i -> a -> f (Maybe c))
    -> IWhenMissing t f i a c

  -- | Traverse over values present in only one side of a merge.
  --
  -- > itraverseMissing f = iwitherMissing (\i a -> Just <$> f i a)
  itraverseMissing :: Applicative f => (i -> a -> f c) -> IWhenMissing t f i a c
  itraverseMissing f = iwitherMissing (\i a -> Just <$> f i a)

  -- | Filter values present in only one side of a merge, using an
  -- 'Applicative' action.
  --
  -- > ifilterAMissing f
  -- >   = iwitherMissing (\i a -> bool Nothing (Just a) <$> f i a)
  ifilterAMissing
    :: Applicative f
    => (i -> a -> f Bool)
    -> IWhenMissing t f i a a
  ifilterAMissing f
    = iwitherMissing (\i a -> bool Nothing (Just a) <$> f i a)

  -- | Map over entries present on only one side of a merge,
  -- optionally removing some.
  --
  -- > imapMaybeMissing f = iwitherMissing (\i a -> pure $ f i a)
  imapMaybeMissing
    :: Applicative f
    => (i -> a -> Maybe c)
    -> IWhenMissing t f i a c
  imapMaybeMissing f = iwitherMissing (\i a -> pure $ f i a)

  -- | Drop entries present on only one side of a merge.
  --
  -- > idropMissing = iwitherMissing (const . const $ pure Nothing)
  idropMissing :: Applicative f => IWhenMissing t f i a c
  idropMissing = iwitherMissing (const . const $ pure Nothing)

  -- | Retain entries present on only one side of a merge.
  --
  -- > ipreserveMissing = iwitherMissing (const $ pure . Just)
  ipreserveMissing
    :: Applicative f
    => IWhenMissing t f i a a
  ipreserveMissing = iwitherMissing (const $ pure . Just)

  -- | Map over entries persent on only one side of a merge.
  --
  -- > imapMissing f = imapMaybeMissing (\i a -> Just $ f i a)
  imapMissing :: Applicative f => (i -> a -> c) -> IWhenMissing t f i a c
  imapMissing f = imapMaybeMissing (\i a -> Just $ f i a)

  -- | Filter entries present on only one side of a merge.
  --
  -- > ifilterMissing f
  -- >   = imapMaybeMissing (\i a -> bool Nothing (Just a) $ f i a)
  ifilterMissing :: Applicative f => (i -> a -> Bool) -> IWhenMissing t f i a a
  ifilterMissing f = imapMaybeMissing (\i a -> bool Nothing (Just a) $ f i a)


  -- | The fundamental way to construct an 'IWhenMatched'.
  --
  -- Use the given function to combine values from both sides of a
  -- merge through an 'Applicative', and optionally discard the
  -- result.
  --
  -- Other functions that return 'IWhenMatched' are specified as class
  -- methods so instances can replace them with optimised versions.
  izipWithMaybeAMatched
    :: Applicative f
    => (i -> a -> b -> f (Maybe c))
    -> IWhenMatched t f i a b c

  -- | Use the given function to combine a pair of values, one from
  -- each side of a merge, through an 'Applicative'.
  --
  -- > izipWithAMatched f = izipWithMaybeAMatched (\i a b -> Just <$> f i a b)
  izipWithAMatched
    :: Applicative f
    => (i -> a -> b -> f c)
    -> IWhenMatched t f i a b c
  izipWithAMatched f = izipWithMaybeAMatched (\i a b -> Just <$> f i a b)

  -- | Use the given function to combine a pair of values, one from
  -- each side of a merge, and maybe discard the result.
  --
  -- > izipWithMaybeMatched f = izipWithMaybeAMatched (\i a b -> pure $ f i a b)
  izipWithMaybeMatched
    :: Applicative f
    => (i -> a -> b -> Maybe c)
    -> IWhenMatched t f i a b c
  izipWithMaybeMatched f = izipWithMaybeAMatched (\i a b -> pure $ f i a b)

  -- | Use the given function to combine a pair of values, one from
  -- each side of a merge.
  --
  -- > izipWithMatched f = izipWithMaybeMatched (\i a b -> Just $ f i a b)
  izipWithMatched
    :: Applicative f
    => (i -> a -> b -> c)
    -> IWhenMatched t f i a b c
  izipWithMatched f = izipWithMaybeMatched (\i a b -> Just $ f i a b)

type ISimpleWhenMissing t i a c = IWhenMissing t Identity i a c
type ISimpleWhenMatched t i a b c = IWhenMatched t Identity i a b c

-- | Purely merge two structures.
imerge
  :: MergeWithIndex i t
  => ISimpleWhenMissing t i a c
  -- ^ Handle values present on the left but not the right
  -> ISimpleWhenMissing t i b c
  -- ^ Handle values present on the right but not the left
  -> ISimpleWhenMatched t i a b c
  -- ^ Handle values present in both instances
  -> t a
  -> t b
  -> t c
imerge left right match = (runIdentity .) . imergeA left right match

instance MergeWithIndex () Maybe where
  imergeA
    (MaybeWhenMissing left)
    (MaybeWhenMissing right)
    (MaybeWhenMatched match)
    = go

    where
      go (Just a) Nothing = left a
      go Nothing (Just b) = right b
      go (Just a) (Just b) = match a b
      go Nothing Nothing = pure Nothing

  data IWhenMissing Maybe f () a c = MaybeWhenMissing (a -> f (Maybe c))
  data IWhenMatched Maybe f () a b c = MaybeWhenMatched (a -> b -> f (Maybe c))

  irunWhenMissing (MaybeWhenMissing f) = const f
  irunWhenMatched (MaybeWhenMatched f) = const f

  iwitherMissing f = MaybeWhenMissing $ f ()
  izipWithMaybeAMatched f = MaybeWhenMatched $ f ()

instance Ord k => MergeWithIndex k (Map k) where
  imergeA (MapWhenMissing left) (MapWhenMissing right) (MapWhenMatched match)
    = Map.mergeA left right match

  data IWhenMissing (Map k) f k a c
    = MapWhenMissing (Map.WhenMissing f k a c)

  data IWhenMatched (Map k) f k a b c
    = MapWhenMatched (Map.WhenMatched f k a b c)

  irunWhenMissing (MapWhenMissing f) = Map.runWhenMissing f
  irunWhenMatched (MapWhenMatched f) = Map.runWhenMatched f

  iwitherMissing = MapWhenMissing . Map.traverseMaybeMissing
  itraverseMissing = MapWhenMissing . Map.traverseMissing
  ifilterAMissing = MapWhenMissing . Map.filterAMissing
  imapMaybeMissing = MapWhenMissing . Map.mapMaybeMissing
  idropMissing = MapWhenMissing Map.dropMissing
  ipreserveMissing = MapWhenMissing Map.preserveMissing
  imapMissing = MapWhenMissing . Map.mapMissing
  ifilterMissing = MapWhenMissing . Map.filterMissing

  izipWithMaybeAMatched = MapWhenMatched . Map.zipWithMaybeAMatched
  izipWithAMatched = MapWhenMatched . Map.zipWithAMatched
  izipWithMaybeMatched = MapWhenMatched . Map.zipWithMaybeMatched
  izipWithMatched = MapWhenMatched . Map.zipWithMatched

-- This would actually live in semialign-diff, but it's here to show
-- Ryan for now.
diff :: forall i t a . (MergeWithIndex i t, Eq a) => t a -> t a -> t (Maybe a)
diff = imerge
  (imapMissing $ \_ _ -> Nothing)
  (imapMissing $ \_ a -> Just a)
  (izipWithMaybeMatched f)

  where
    f :: Eq a => i -> a -> a -> Maybe (Maybe a)
    f _ old new
      | old == new = Nothing
      | otherwise = Just $ Just new