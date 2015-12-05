module Main where
import Data.List
import System.Environment


-- Utility functions
apply_dir '^' (x, y) = (x, y + 1)
apply_dir 'v' (x, y) = (x, y - 1)
apply_dir '<' (x, y) = (x - 1, y)
apply_dir '>' (x, y) = (x + 1, y)
apply_dir x _ = error ("Invalid input " ++ [x])

build_path insts = build (0,0) insts
  where build pos [] = [pos]
        build pos (h:t) = pos:(build (apply_dir h pos) t)

set [] = []
set (h:t) = h:(set $ filter (/= h) t)

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xys) = (x:xs, y:ys) where (xs, ys) = split xys

-- Part 1
houses insts = set $ build_path insts

-- Part 2
houses_with_robot (santa, robot) = set $ (build_path santa)
                                   ++ (build_path robot)

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn ("Part 1: " ++ (show $ length $ houses content))
  let sr = split content
  putStrLn ("Part 2: " ++ (show $ length $ houses_with_robot sr))
