module MyArray(Ix (..), Array, array, listArray, update, (//), (!), elems) where

class Ord a => Ix a where
  range :: (a, a) -> [a]
  
  index :: (a, a) -> a -> Int
  index r i
    | inRange r i =  length [x | x <- range r, x < i] 
    | otherwise = error "Index out of range"
  
  inRange :: (a, a) -> a -> Bool
  inRange (beg, end) i = (i >= beg) && (i <= end)

  rangeSize :: (a, a) -> Int
  rangeSize = length . range

instance Ix Char where
  range (beg, end) = [beg..end]

instance Ix Int where
  range (beg, end) = [beg..end]
  
  index r@(beg, _) i
    | inRange r i = beg - i
    | otherwise = error "Index out of range"
      
  rangeSize (beg, end) = max 0 $ end - beg + 1

instance Ix Integer where
  range (beg, end) = [beg..end]
  
  index r@(beg, _) i
    | inRange r i = fromInteger $ i - beg
    | otherwise = error "Index out of range"
      
  rangeSize (beg, end) = max 0 $ fromInteger $ end - beg + 1

instance (Ix a, Ix b) => Ix (a, b) where
  range ((begA, begB), (endA, endB)) =
    [(x, y) | x <- range (begA, endA), y <- range (begB, endB)]

  index r@((begA, begB), (endA, endB)) (x, y)
    | inRange r (x, y) = index r1 x * rangeSize r2 + index r2 y
    | otherwise = error "Index out of range"
    where
      r1 = (begA, endA)
      r2 = (begB, endB)

  inRange ((begA, begB), (endA, endB)) (x, y) =
    inRange (begA, endA) x && inRange (begB, endB) y 

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

-- Auxiliary recursive function for creating array
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
listArray ran l = array ran $ zip (range ran) l

(!) :: (Ix i) => Array i e -> i -> e
(!) (ran, arr) ind
  | inRange ran ind =  (!!!) arr (index ran ind)
  | otherwise = error "Index out of range"

-- Auxiliary recursive function for finding element
(!!!) :: ArrayAux e -> Int -> e
(!!!) (Leaf  e) _ = e
(!!!) EmptyNode _ = error "No value assigned"
(!!!) (Node mid left right) ind
  | ind <= mid = (!!!) left ind
  | otherwise = (!!!) right ind

elems :: Ix i => Array i e -> [e]
elems (_, arr) = elemsAux arr []

-- Auxiliary recursive function for creating list of elements.
-- Uses accumulator to avoid lists concatenation/
elemsAux :: ArrayAux e -> [e] -> [e]
elemsAux (Leaf el) acc = el:acc
elemsAux EmptyNode acc = acc
elemsAux (Node _ left right) acc =
  elemsAux left newAcc
  where
    newAcc = elemsAux right acc

update :: Ix i => i -> e -> Array i e -> Array i e
update ind el (r@(beg, end), arr)
  | inRange r ind = (r, updateAux (0, rangeSize r - 1) (index (beg, end) ind) el arr) 
  | otherwise = error "Index out of range"

-- Auxiliary recursive function for updating element
updateAux :: (Int, Int) -> Int -> e -> ArrayAux e -> ArrayAux e
updateAux _ _ el (Leaf _) = Leaf  el
updateAux (beg, end) ind el EmptyNode =
  if beg == end then
    Leaf el
  else
    Node mid newLeft newRight
  where
    mid = beg + (end - beg + 1) ` div` 2 - 1
    newLeft = if ind <= mid
              then updateAux (beg, mid) ind el EmptyNode
              else EmptyNode
    newRight = if ind > mid
               then updateAux (mid + 1, end) ind el EmptyNode
               else EmptyNode
updateAux (beg, end) ind el (Node mid left right) =
  Node mid newLeft newRight
  where
    newLeft = if ind <= mid
              then updateAux (beg, mid) ind el left
              else left
    newRight = if ind > mid
               then updateAux (mid + 1, end) ind el right
               else right

(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) = foldl (\ acc (ind, el) -> update ind el acc)
