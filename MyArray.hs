module MyArray where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  
  index :: (a, a) -> a -> Int
  index (beg, end) i =
    if inRange (beg, end) i then
      head [ n | (x, n) <-
            zip (range (beg, end)) [0..(rangeSize(beg, end))],
            x == i  ]
    else
      error "Index out of range"
      
  
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

instance (Ix a, Ix b) => Ix (a, b) where
  range ((begA, begB), (endA, endB)) =
    zip (range (begA, endA)) (range (begB, endB))
  
type Array i e = [(i, e)]

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (beg, end) = zip (range (beg, end))

(!) :: (Ix i) => Array i e -> i -> e
(!) arr ind = head [y | (x,y) <- arr, x == ind]

elems :: Ix i => Array i e -> [e]
elems arr = [y | (x, y) <- arr]

-- czy tylko część wybieramy?
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array (beg, end) l =
  [(x, y) | (x, y) <- l, inRange (beg, end) x]

update :: Ix i => i -> e -> Array i e -> Array i e
update ind el arr =
  (ind, el):[(x, y) | (x, y) <- arr, x /= ind]

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) arr l = foldl (\ acc (ind, el) -> update ind el acc) arr l
