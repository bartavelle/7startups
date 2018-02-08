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
  , RMultiSet.null
  ) where

import qualified Data.Set as S
import Data.Aeson
import Data.Bits
import Data.Word
import Data.List (foldl')

import Startups.Base

newtype ResourceSet
    = ResourceSet Word64
    deriving (Eq, Ord)

instance Show ResourceSet where
    show = show . toList

instance Monoid ResourceSet where
    mempty = ResourceSet 0
    {-# INLINE mempty #-}
    mappend (ResourceSet a) (ResourceSet b) = ResourceSet (a + b)
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

null :: ResourceSet -> Bool
null (ResourceSet r) = r == 0
{-# INLINE null #-}

occur :: Resource -> ResourceSet -> Int
occur r (ResourceSet s) = fromIntegral ((s `shiftR` (fromEnum r * 8)) .&. 0xff)
{-# INLINE occur #-}

toOccurList :: ResourceSet -> [(Resource, Int)]
toOccurList s = [ (r, occur r s) | r <- resources ]
{-# INLINE toOccurList #-}

fromOccurList :: [(Resource, Int)] -> ResourceSet
fromOccurList = foldr (uncurry insertMany) mempty
{-# INLINE fromOccurList #-}

foldMap :: Monoid m => (Resource -> m) -> ResourceSet -> m
foldMap f = Prelude.foldMap f . toList
{-# INLINE foldMap #-}

insertMany :: Resource -> Int -> ResourceSet -> ResourceSet
insertMany r n (ResourceSet s) = ResourceSet (s + (fromIntegral n `shiftL` (fromEnum r * 8)))
{-# INLINE insertMany #-}

insert :: Resource -> ResourceSet -> ResourceSet
insert r (ResourceSet s) = ResourceSet (s + (1 `shiftL` (fromEnum r * 8)))
{-# INLINE insert #-}

isSubsetOf :: ResourceSet -> ResourceSet -> Bool
isSubsetOf (ResourceSet s1) (ResourceSet s2) =
    all check [0..6]
  where
    check n =
      let !mask = 0xff `shiftL` (n * 8)
      in  s1 .&. mask <= s2 .&. mask
{-# INLINE isSubsetOf #-}

member :: Resource -> ResourceSet -> Bool
member r (ResourceSet s) = (s `shiftR` (fromEnum r * 8)) .&. 0xff > 0
{-# INLINE member #-}

delete :: Resource -> ResourceSet -> ResourceSet
delete r (ResourceSet s) = ResourceSet modified
  where
    !idx = fromEnum r * 8
    !mask = 0xff `shiftL` idx
    !imask = complement mask
    !cur = (s `shiftR` idx) .&. 0xff
    !new = if cur == 0
             then 0
             else cur - 1
    !modified = (s .&. imask) .|. (new `shiftL` idx)
{-# INLINE delete #-}

difference :: ResourceSet -> ResourceSet -> ResourceSet
difference (ResourceSet s1) (ResourceSet s2) =
    ResourceSet (foldl' foo 0 [0..6])
  where
    foo cur i =
      let !mask = 0xff `shiftL` (i * 8)
          !m1 = s1 .&. mask
          !m2 = s2 .&. mask
      in  if m2 >= m1
            then cur
            else cur .|. (m1 - m2)
{-# INLINE difference #-}

toSet :: ResourceSet -> S.Set Resource
toSet = S.fromList . toList
{-# INLINE toSet #-}

instance ToJSON ResourceSet where
    toJSON = toJSON . toOccurList

instance FromJSON ResourceSet where
    parseJSON = fmap fromOccurList . parseJSON

