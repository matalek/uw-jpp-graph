import System.Environment

import MyGraph

smain :: String -> String

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> interact smain
    (file:_) -> do
      s <- readFile file
      putStrLn $ smain s

smain input =
  let
    neighboursLists = foldl parseLine [] $ lines input
    vertices = [x | (x,_) <- neighboursLists]
    beg = minimum vertices
    end = maximum vertices
    g = graph (beg, end) neighboursLists
    vis = dfs g 1
  in
    if (null vertices) || (1 `notElem` vertices) then
      "[]"
    else
      show vis

parseLine :: [(Int,[Int])] -> String -> [(Int,[Int])]
parseLine g l = parse g $ words l

parse :: [(Int,[Int])] -> [String] -> [(Int,[Int])] 
parse g [] = g
parse g (v:neighbours) =
  (read v :: Int, listToInts neighbours):g
  where
    listToInts = map (\ el -> read el :: Int)
