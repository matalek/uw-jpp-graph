module MyArray where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  
  index :: (a, a) -> a -> Int
  index (beg, end) i =
    head [ n | (x, n) <- zip (range (beg, end)) ([0..(rangeSize(beg, end))]), x == i  ]
  
  inRange :: (a, a) -> a -> Bool
  inRange (beg, end) i = (i >= beg) && (i <= end)

  rangeSize :: (a, a) -> Int
  rangeSize = length . range

instance Ix Char where
  range (beg, end) = [beg..end]

instance Ix Int where
  range (beg, end) = [beg..end]
  index (beg, _) i = i - beg

instance Ix Integer where
  range (beg, end) = [beg..end]

-- instance (Ix a, Ix b) => Ix (a, b) where
