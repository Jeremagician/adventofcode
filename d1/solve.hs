module Main where
import System.Environment
import Data.List

relshift :: Char -> Int
relshift '(' = 1
relshift ')' = -1
relshift x = error ("Invalid char " ++ [x])

-- Part 1
end_lvl xs = foldl (\x y-> x + (relshift y)) 0 xs

-- Part 2
basement_index xs = findIndex (== -1) (scanl (\x y-> x + (relshift y)) 0 xs)


main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn ("Part 1: " ++ (show $ end_lvl content))
  putStrLn ("Part 2: " ++ (show $ basement_index content))
