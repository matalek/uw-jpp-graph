module MyGraph where
import MyArray

type Graph = Array Int [Int]

dfs :: Graph -> Int -> [Int]
dfs g v = dfsAux g v []

dfsAux :: Graph -> Int -> [Int] -> [Int]
dfsAux g v vis = 
  foldl visit (v:vis) $ g ! v
  where
    visit l u =
      if u `elem` l then
        l
      else
        dfsAux g u l
