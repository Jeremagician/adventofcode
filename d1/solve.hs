module Main where
import System.Environment

relshift :: Char -> Int
relshift '(' = 1
relshift ')' = -1
relshift x = error ("Invalid char " ++ [x])

-- Part 1
end_lvl xs = foldl (\x y-> x + (relshift y)) 0 xs

-- Part 2
basement_index xs = findBasement 0 1 xs
  where findBasement _ _ [] = error "Santa never goes to the basement!"
        findBasement start idx (h:t) =
          let newlvl = start + (relshift h)
          in if newlvl == (-1) then idx else findBasement newlvl (idx + 1) t

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn ("Part 1: " ++ (show $ end_lvl content))
  putStrLn ("Part 2: " ++ (show $ basement_index content))
