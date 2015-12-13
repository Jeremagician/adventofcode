module Main where
import qualified Data.Map as M
import System.Environment
import Data.List
import Data.Tuple

-- YET ANOTHER INEFICIENT SOLUTION
-- ... WORKING ON IT

parse_lines :: String -> ((String, String), Int)
parse_lines ln = case (words (init ln)) of
  [x, "would", act, amnt, "happiness", "units",
   "by", "sitting", "next", "to", y]
    -> ((x, y), (if act == "gain" then 1 else -1) * (read amnt))
  _ -> undefined

parse :: String -> [((String, String), Int)]
parse = map parse_lines . lines

score cs (x,y) = if x == "me" || y == "me"
                 then 0
                 else (cs M.! (x,y)) + (cs M.! (y,x))

topology_happiness cs xs = sum $ map (score cs) pairs
  where pairs = zip xs ((drop 1 xs) ++ (take 1 xs))

solve cstrs people = maximum $ map (topology_happiness cstrs) topologies
  where topologies = permutations people

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let cstrs = M.fromList $ parse content
  let people = nub $ map fst (M.keys cstrs)
  putStrLn $ "Part 1: " ++ (show $ solve cstrs people)
  let everybody = "me":people
  putStrLn $ "Part 2: " ++ (show $ solve cstrs everybody)
