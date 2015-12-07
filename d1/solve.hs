module Main where
import System.Environment
import Data.List

apply x '(' = x+1
apply x ')' = x-1
apply x y = error $ "Unexpected char " ++ [y]

-- Part 1
end_lvl xs = foldl apply 0 xs

-- Part 2
basement_index xs = findIndex (== -1) (scanl apply 0 xs)


main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn ("Part 1: " ++ (show $ end_lvl content))
  putStrLn ("Part 2: " ++ (show $ basement_index content))
