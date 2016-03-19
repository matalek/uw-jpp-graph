module MyGraph(dfs, graph) where
import MyArray
import Data.Set as Set

type Graph = ((Int, Int), Array Int [Int])

graph :: (Int, Int) -> [(Int, [Int])] -> Graph
graph ran@(beg, end) l =
  (ran, array ran newList)
  where
    newList = l ++ [(x, []) | x <- [beg..end]]

dfs :: Graph -> Int -> [Int]
dfs g v = Set.elems $ dfsAux g v empty

dfsAux :: Graph -> Int -> Set.Set Int -> Set.Set Int
dfsAux g@(ran, arr) v vis =
  if inRange ran v then
    Prelude.foldl visit (insert v vis) $ arr ! v
  else
    insert v vis
  where
    visit s u =
      if member u s then
        s
      else
        dfsAux g u s
