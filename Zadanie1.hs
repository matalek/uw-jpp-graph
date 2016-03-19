import System.Environment
import System.Directory

import MyGraph

smain :: String -> String

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> interact smain
    (file:_) -> do
      fileExists <- doesFileExist file
      if fileExists then do
         s <- readFile file
         putStrLn $ smain s
        else
          putStrLn "File does not exist"

smain input =
  let
    neighboursLists = foldl parseLine [] $ lines input
    vertices = [x | (x,_) <- neighboursLists]
    beg = minimum vertices
    end = maximum vertices
    g = graph (beg, end) neighboursLists
    vis = dfs g 1
  in
    if vertices == [] then
      "[]"
    else
      show vis

parseLine :: [(Int,[Int])] -> String -> [(Int,[Int])]
parseLine g l = parse g $ words l

parse :: [(Int,[Int])] -> [String] -> [(Int,[Int])] 
parse g [] = g
parse g (v:neighbours) =
  (read v :: Int ,listToInts neighbours):g
  where
    listToInts = map (\ el -> read el :: Int)
