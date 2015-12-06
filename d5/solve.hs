module Main where
import Data.List
import System.Environment

-- Part 1
vowel_count = length . filter (`elem` "aeiou")
has_twice_char x = find (\x -> fst x == snd x) (zip x (tail x)) /= Nothing
no_forbidden x = and $ map (not . (`isInfixOf` x)) blacklist
  where blacklist = ["ab", "cd", "pq", "xy"]

cond_old_nice x = [vowel_count x > 2, has_twice_char x, no_forbidden x]

-- Part 2
has_pair x = check (zip x (tail x))
  where check [] = False
        check [_] = False
        check (y:ys)
          | elem y (tail ys) = True
          | otherwise = check ys

is_wrapped (x, _ , z) = x == z
has_wrapped x =  or $ map is_wrapped (zip3 x (tail x) (drop 2 x))

cond_new_nice x = [has_pair x, has_wrapped x]



count_nice cond content = length $ filter (== True)
                          $ map (and . cond) $ lines content

main = do
  args <- getArgs
  content <- readFile (head args)
  putStrLn $ "Part 1: " ++ (show (count_nice cond_old_nice content))
  putStrLn $ "Part 2: " ++ (show (count_nice cond_new_nice content))
