module Main where
import System.Environment
import Data.List

parse_replacement :: String -> (String, String)
parse_replacement ln = case words ln of
  [src, "=>", dst] -> (src, dst)
  _ -> error ("Bad replacement: " ++ ln)

load_input :: IO ([(String, String)], String)
load_input = getArgs >>= readFile . (!! 0)
             >>= return . process . span ((/= 0) . length) . lines
  where process (r, m) = (map parse_replacement r, m !! 1)


replace molecule rep@(src,dest) = traverse [] molecule
  where traverse _ [] = []
        traverse start end =
          if src `isPrefixOf` end
          then (start ++ dest ++ (drop (length src) end)):cont
          else cont
          where cont = traverse (start ++ [head end]) (tail end)

all_reps reps molecule = concatMap (replace molecule) reps
calibrate reps molecule = length $ nub $ all_reps reps molecule

main :: IO ()
main = do
  (reps, molecule) <- load_input
  print $ calibrate reps molecule
