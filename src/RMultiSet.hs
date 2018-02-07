{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module RMultiSet
  ( ResourceSet
  , singleton
  , toList
  , fromList
  , toOccurList
  , fromOccurList
  , RMultiSet.foldMap
  , insertMany
  , isSubsetOf
  , member
  , insert
  , delete
  , difference
  , toSet
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Set as S
import Data.Aeson

import Startups.Base

newtype ResourceSet
    = ResourceSet { getMultiSet :: V.Vector Int }
    deriving (Eq, Ord)

instance Show ResourceSet where
    show = show . toList

instance Monoid ResourceSet where
    mempty = ResourceSet (V.replicate 7 0)
    {-# INLINE mempty #-}
    mappend (ResourceSet a) (ResourceSet b) = ResourceSet (V.zipWith (+) a b)
    {-# INLINE mappend #-}

singleton :: Resource -> ResourceSet
singleton r = insert r mempty
{-# INLINE singleton #-}

fromList :: [Resource] -> ResourceSet
fromList = foldr insert mempty
{-# INLINE fromList #-}

resources :: [Resource]
resources = [minBound .. maxBound]

toList :: ResourceSet -> [Resource]
toList = concatMap (uncurry (flip replicate)) . toOccurList
{-# INLINE toList #-}

toOccurList :: ResourceSet -> [(Resource, Int)]
toOccurList = zip resources . V.toList . getMultiSet
{-# INLINE toOccurList #-}

fromOccurList :: [(Resource, Int)] -> ResourceSet
fromOccurList = foldr (uncurry insertMany) mempty
{-# INLINE fromOccurList #-}

foldMap :: Monoid m => (Resource -> m) -> ResourceSet -> m
foldMap f = Prelude.foldMap f . toList
{-# INLINE foldMap #-}

insertMany :: Resource -> Int -> ResourceSet -> ResourceSet
insertMany r n (ResourceSet s) = ResourceSet (V.modify im s)
  where
    im t = VM.unsafeModify t (+n) (fromEnum r)
{-# INLINE insertMany #-}

insert :: Resource -> ResourceSet -> ResourceSet
insert r (ResourceSet s) = ResourceSet (V.modify im s)
  where
    im t = VM.unsafeModify t (+1) (fromEnum r)
{-# INLINE insert #-}

isSubsetOf :: ResourceSet -> ResourceSet -> Bool
isSubsetOf (ResourceSet s1) (ResourceSet s2) =
    not (V.any (< 0) (V.zipWith (-) s2 s1))
{-# INLINE isSubsetOf #-}

member :: Resource -> ResourceSet -> Bool
member r (ResourceSet s) = s V.! fromEnum r > 0
{-# INLINE member #-}

delete :: Resource -> ResourceSet -> ResourceSet
delete r (ResourceSet s) = ResourceSet (V.modify im s)
  where
    im t = VM.unsafeModify t (\x -> max 0 (x-1)) (fromEnum r)
{-# INLINE delete #-}

difference :: ResourceSet -> ResourceSet -> ResourceSet
difference (ResourceSet s1) (ResourceSet s2) =
    ResourceSet (V.zipWith (\a b -> max 0 (a - b)) s1 s2)
{-# INLINE difference #-}

toSet :: ResourceSet -> S.Set Resource
toSet = S.fromList . toList
{-# INLINE toSet #-}

instance ToJSON ResourceSet where
    toJSON = toJSON . toOccurList

instance FromJSON ResourceSet where
    parseJSON = fmap fromOccurList . parseJSON

