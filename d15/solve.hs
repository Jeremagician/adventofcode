module Main where
import System.Environment
import Data.List
import Control.Monad

parse_line :: String -> [Integer]
parse_line ln = case words (filter (/=',') ln) of
  [_, "capacity", cap, "durability", dur, "flavor", flav,
   "texture", tex, "calories", cal]
    -> [(read cap), (read dur), (read flav), (read tex), (read cal)]
  _ -> error $ "Bad input line: \"" ++ ln ++"\""


-- too bruteforcy, find a better way to generate all this
spoon_combination :: Int -> [[Integer]]
spoon_combination l = filter ((== 100) . sum) $ replicateM l [1..99]

pos x
  | x < 0 = 0
  | otherwise = x

-- [[ing1], [ing2]]
partial_sum ing recipe = map (pos . sum) $ transpose melange
  where dosage = zip ing recipe
        melange = map (\(x, f) -> (map (*f) x)) dosage

--                                 Recipe score    Total Calories
--                                     vvvvv         vvvvvv
all_recipe_score ing = map (\x -> (product (init x), last x)) all_melanges
  where all_recipes = spoon_combination $ length ing
        all_melanges = map (partial_sum ing) all_recipes

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let ingredients = map parse_line $ lines content
  let scores = all_recipe_score ingredients
  putStrLn $ "Part 1: " ++ (show $ maximum $ map fst scores)
  putStrLn $ "Part 2: " ++ (show $ maximum $ map fst
                            $ filter ((== 500) . snd) scores)
