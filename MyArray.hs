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
  rangeSize (beg, end) = end - beg + 1

instance Ix Integer where
  range (beg, end) = [beg..end]
  index (beg, _) i = fromInteger $ i - beg
  rangeSize (beg, end) = fromInteger $ end - beg + 1

instance (Ix a, Ix b) => Ix (a, b) where
  range ((begA, begB), (endA, endB)) =
    [(x, y) | x <- (range (begA, endA)), y <- (range (begB, endB))]

  rangeSize ((begA, begB), (endA, endB)) =
    (rangeSize (begA, endA)) * (rangeSize (begB, endB))
                                                      
data Array i e = Leaf (Maybe e) | Node (i, i) i (Array i e) (Array i e) deriving (Show)

array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array (beg, end) = arrayAux (range (beg, end)) 

arrayAux :: (Ix i) => [i] -> [(i, e)] -> Array i e
arrayAux [] _ = Leaf Nothing
arrayAux r l 
  | beg == end = createLeaf l
  | otherwise =  Node (beg, end) mid left right
  where
    beg = head r
    end = last r
    createLeaf [] = Leaf Nothing
    createLeaf ((_, el):_) = Leaf (Just el)
    halfSize = (length r) ` div` 2 - 1 -- nie wiem, czy to ma sens
    mid = r !! halfSize
    left = arrayAux (filter (<=mid) r) $ filter (\ (ind, _) -> ind <= mid) l
    right = arrayAux (filter (>mid) r) $ filter (\ (ind, _) -> ind > mid) l

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (beg, end) l = array (beg, end) $ zip (range (beg, end)) l

(!) :: (Ix i) => Array i e -> i -> e
(!) (Leaf (Just e)) _ = e
(!) (Node _ mid left right) i
  | i <= mid = (!) left i
  | otherwise = (!) right i

elems :: Ix i => Array i e -> [e]
elems arr = elemsAux arr []

elemsAux :: Ix i => Array i e -> [e] -> [e]
elemsAux (Leaf (Just el)) acc = (el:acc)
elemsAux (Leaf Nothing) acc = acc
elemsAux (Node _ _ left right) acc =
  elemsAux left newAcc
  where
    newAcc = elemsAux right acc

update :: Ix i => i -> e -> Array i e -> Array i e
update _ el (Leaf _) = Leaf (Just el)
update ind el (Node (beg, end) mid left right) =
  Node (beg, end) mid newLeft newRight
  where
    newLeft = if ind <= mid then update ind el left else left
    newRight = if ind > mid then update ind el right else right

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) arr l = foldl (\ acc (ind, el) -> update ind el acc) arr l
