import MyArray
import MyGraph

smain :: String -> String
main = interact smain

smain input =
  let
    ls = lines input
    parseLine g l = parse g $ words l
    listToInts = map (\ el -> read el :: Int)
    parse g (v:neighbours) =
      (read v :: Int ,listToInts neighbours):g
    neighboursLists = foldl parseLine [] ls
    vertices = [x | (x,y) <- neighboursLists]
    beg = minimum vertices
    end = maximum vertices
    graph = (array (beg, end) neighboursLists) :: Graph
    vis = dfs graph 1
  in
    show vis



