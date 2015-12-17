module Main where
import System.Environment
import Data.List
import Data.Ord

parse :: String -> [Int]
parse c = map read $ lines c

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let sol = filter ((== 150) . sum) $ subsequences $ parse content
  print $ length sol
  let min_len = length $ minimumBy (comparing length) sol
  print $ length $ filter ((== min_len) . length) sol
