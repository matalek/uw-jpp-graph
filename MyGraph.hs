module MyGraph(dfs, graph) where
import MyArray
import Data.Set as Set

type Graph = Array Int [Int]

graph :: (Int, Int) -> [(Int, [Int])] -> Graph
graph = array

dfs :: Graph -> Int -> [Int]
dfs g v = Set.elems $ dfsAux g v empty

dfsAux :: Graph -> Int -> Set.Set Int -> Set.Set Int
dfsAux g v vis =
  Prelude.foldl visit (Set.insert v vis) $ g ! v
  where
    visit s u
      | Set.member u s = s
      | otherwise = dfsAux g u s
