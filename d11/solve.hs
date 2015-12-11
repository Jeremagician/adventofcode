module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Set hiding (filter, map)


-- double reverse is not very efficient.
next str = reverse $ next' $ reverse str
  where next' [] = undefined
        next' (x:xs) = (nextChar x):(if x == 'z' then next' xs else xs)
        nextChar c = chr $ ((1 + ord c - ord 'a') `mod` 26) + ord 'a'

three_succ :: String -> Bool
three_succ = or . map is_valid . filter ((== 3) . length) . map (take 3) . tails
  where is_valid [] = True
        is_valid [x] = True
        is_valid (a:(b:rs)) = if (succ a) /= b then False else is_valid (b:rs)

no_forbidden = and . map (not . (`elem` "iol"))
has_two_pair x = length (fromList pairs) > 1
  where pairs = filter (\x -> fst x == snd x) (zip x $ tail x)

valid_pass x = and $ map (\pred -> pred x) [has_two_pair, no_forbidden, three_succ]

main = do
  args <- getArgs
  let next_passwds = (args !! 0):[next x | x <- next_passwds]
  let valids = filter valid_pass next_passwds
  putStrLn $ "Part 1 " ++ (show $ valids !! 0)
  putStrLn $ "Part 2 " ++ (show $ valids !! 1)
