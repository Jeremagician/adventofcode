module Main where
import Data.List.Ordered
import System.Environment

to_remove ':' = True
to_remove ',' = True
to_remove _ = False

--             Line    Sue num   Properties
parse_line :: String -> (Int, [(String, Int)])
parse_line ln = case words (filter (not . to_remove) ln) of
  ["Sue", num, p1, v1, p2, v2, p3, v3]
    -> (read num, sort [(p1, read v1), (p2, read v2), (p3, read v3)])
  _ -> error $ "Bad input line: \"" ++ ln ++"\""


analysed_data :: [(String, Int)]
analysed_data = sort [("children", 3)
                ,("cats", 7)
                ,("samoyeds", 2)
                ,("pomeranians", 3)
                ,("akitas", 0)
                ,("vizslas", 0)
                ,("goldfish", 5)
                ,("trees", 3)
                ,("cars", 2)
                ,("perfumes", 1)]

correct (p1, v1) (p2, v2)
  | p1 == p2 =
    case p1 of
      "cats"        -> if v1 > v2 then EQ else LT
      "trees"       -> if v1 > v2 then EQ else LT
      "pomeranians" -> if v1 < v2 then EQ else GT
      "goldfish"    -> if v1 < v2 then EQ else GT
      _ -> compare (p1,v1) (p2, v2)
  | otherwise = compare (p1,v1) (p2, v2)

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let remembered = map parse_line $ lines content
  let candidate = filter (\(i, d) -> d `subset` analysed_data) remembered
  putStrLn $ "Part 1: " ++ show candidate
  let new_candidate = filter (\(i, d) -> subsetBy correct d analysed_data) remembered
  putStrLn $ ("Part 2: " ++ (show new_candidate))
