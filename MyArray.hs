module MyArray(Ix (..), Array, array, listArray, update, (//), (!), elems) where

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
    rangeSize (begA, endA) * rangeSize (begB, endB)

-- Array type

type Array i e = ((i, i), ArrayAux e)

data ArrayAux e = Leaf e | Node Int (ArrayAux e) (ArrayAux e) | EmptyNode deriving (Show)

array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array ran l = let
  toInsert = map (\(ind, el) -> (index ran ind, el)) l
  in
    if rangeSize ran > 0 then
      (ran, arrayAux (0, rangeSize ran - 1) toInsert) 
    else
      (ran, EmptyNode)

arrayAux :: (Int, Int) -> [(Int, e)] -> ArrayAux e
arrayAux _ [] = EmptyNode
arrayAux (beg, end) l@((_, el):_)
  | beg == end = Leaf  el
  | otherwise =  Node mid left right
  where
    mid = beg + (end - beg + 1) ` div` 2 - 1
    left = arrayAux (beg, mid) $ filter (\ (ind, _) -> ind <= mid) l
    right = arrayAux (mid + 1, end) $ filter (\ (ind, _) -> ind > mid) l

listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (beg, end) l = array (beg, end) $ zip (range (beg, end)) l

(!) :: (Ix i) => Array i e -> i -> e
(!) ((beg, end), arr) ind
  | inRange (beg, end) ind =  (!!!) arr (index (beg, end) ind)
  | otherwise = error "Index out of range"
    
(!!!) :: ArrayAux e -> Int -> e
(!!!) (Leaf  e) _ = e
(!!!) EmptyNode _ = error "No value assigned"
(!!!) (Node mid left right) ind
  | ind <= mid = (!!!) left ind
  | otherwise = (!!!) right ind

elems :: Ix i => Array i e -> [e]
elems (_, arr) = elemsAux arr []

elemsAux :: ArrayAux e -> [e] -> [e]
elemsAux (Leaf el) acc = el:acc
elemsAux EmptyNode acc = acc
elemsAux (Node _ left right) acc =
  elemsAux left newAcc
  where
    newAcc = elemsAux right acc

update :: Ix i => i -> e -> Array i e -> Array i e
update ind el old@(r@(beg, end), arr)
  | inRange r ind = (r, updateAux (0, rangeSize r - 1) (index (beg, end) ind) el arr) 
  | otherwise = old
    
updateAux :: (Int, Int) -> Int -> e -> ArrayAux e -> ArrayAux e
updateAux _ _ el (Leaf _) = Leaf  el
updateAux (beg, end) ind el EmptyNode =
  if beg == end then
    Leaf el
  else
    Node mid newLeft newRight
  where
    mid = beg + (end - beg + 1) ` div` 2 - 1
    newLeft = if ind <= mid then updateAux (beg, mid) ind el EmptyNode
              else EmptyNode
    newRight = if ind > mid then updateAux (mid+1, end) ind el EmptyNode
               else EmptyNode
   
updateAux (beg, end) ind el (Node mid left right) =
  Node mid newLeft newRight
  where
    newLeft = if ind <= mid then updateAux (beg, mid) ind el left else left
    newRight = if ind > mid then updateAux (mid + 1, end) ind el right else right

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) = foldl (\ acc (ind, el) -> update ind el acc)
