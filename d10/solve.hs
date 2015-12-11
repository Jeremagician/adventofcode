module Main where
import Data.List
import System.Environment

look_and_say [] = []
look_and_say (x:xs) = (show (1 + length same)) ++ [x] ++ look_and_say rest
  where (same, rest) = span (==x) xs

all_look_and_say start = iterate look_and_say start
len_at start idx = length $ all_look_and_say start !! idx

main = do
  args <- getArgs
  putStrLn $ "Part 1: " ++ (show $ len_at (args !! 0) 40)
  putStrLn $ "Part 2: " ++ (show $ len_at (args !! 0) 50)
