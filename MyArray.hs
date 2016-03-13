module MyArray where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  index :: (a, a) -> a -> Int
  inRange :: (a, a) -> a -> Bool

  rangeSize :: (a, a) -> Int
  rangeSize = length . range

instance Ix Int where
  range (beg, end) = [beg..end]
  index (beg, _) i = i - beg
  inRange (beg, end) i = (i >= beg) || (i <= end)
  
