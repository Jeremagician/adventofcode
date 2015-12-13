module Main where
import qualified Data.Map as M
import System.Environment
import Data.List
import Data.Tuple

parse_lines :: String -> ((String, String), Int)
parse_lines ln = case (words (init ln)) of
  [x, "would", act, amnt, "happiness", "units",
   "by", "sitting", "next", "to", y]
    -> ((x, y), (if act == "gain" then 1 else -1) * (read amnt))
  _ -> undefined

parse :: String -> [((String, String), Int)]
parse = map parse_lines . lines

topology_happiness cs xs = sum $ map (\x -> (cs M.! x) + (cs M.! (swap x))) pairs
  where pairs = zip xs ((drop 1 xs) ++ (take 1 xs))

solve cstrs = maximum $ map (topology_happiness cstrs) topologies
  where people = nub $ map fst (M.keys cstrs)
        topologies = permutations people

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let cstrs = M.fromList $ parse content
  putStrLn $ show $ solve cstrs
