module MyGraph(dfs, graph) where
import MyArray
import Data.Set as Set

type Graph = Array Int [Int]

-- Graph constructor
graph :: (Int, Int) -> [(Int, [Int])] -> Graph
graph = array

-- Returns list of vertices reachable from the vertex.
-- Uses DFS algorithm
dfs :: Graph -> Int -> [Int]
dfs g v = Set.elems $ dfsAux g v empty

-- Auxiliary recursive function for DFS traversing
dfsAux :: Graph -> Int -> Set.Set Int -> Set.Set Int
dfsAux g v vis =
  Prelude.foldl visit (Set.insert v vis) $ g ! v
  where
    visit s u
      | Set.member u s = s
      | otherwise = dfsAux g u s
