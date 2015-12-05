module Main where
import Data.List
import Data.List.Split
import System.Environment

-- Parse the input to into [[l,w,h]]
parse :: String -> [[Int]]
parse dat = map (map read . splitOn "x") (lines dat)

-- Part 1
rot xs = (drop 1 xs ++ take 1 xs)
surface dim = sum $ map (* 2) $ zipWith (*) dim (rot dim)
bonus dim = foldl (*) 1 (init (sort dim))

total_paper dat = sum $ map (\x -> surface x + bonus x) dat

-- Part 2
ribbon_wrap dim = sum $ smallest ++ smallest
  where smallest = (init (sort dim))
total_ribbon dat = sum $ map (\x -> ribbon_wrap x + (product x)) dat

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let dat = parse content
  putStrLn $ "Part 1: " ++ show (total_paper dat)
  putStrLn $ "Part 2: " ++ show (total_ribbon dat)
