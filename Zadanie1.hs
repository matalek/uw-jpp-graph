import System.Environment

import MyArray
import MyGraph

smain :: String -> String

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> interact smain
    (file:_) -> do
      s <- readFile file
      print $ smain s


smain input =
  let
    ls = lines input
    parseLine g l = parse g $ words l
    listToInts = map (\ el -> read el :: Int)
    parse g [] = g
    parse g (v:neighbours) =
      (read v :: Int ,listToInts neighbours):g
    neighboursLists = foldl parseLine [] ls
    vertices = [x | (x,_) <- neighboursLists]
    beg = minimum vertices
    end = maximum vertices
    graph = (array (beg, end) neighboursLists) :: Graph
    vis = dfs graph 1
  in
    show vis



