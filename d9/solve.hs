module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Data.Tuple
import Data.List
import qualified Data.Set as S
import Control.Monad


type City = String
type Distance = Int
newtype Way = Way (City, City)

instance Eq Way where
  Way wa == Way wb = wa == wb || wa == (swap wb)

instance Show Way where
  show (Way (c1, c2)) = c1 ++ " -> " ++ c2

mkway c1 c2 = Way (c1, c2)

-- Parsing stuff
dist :: Parser Distance
dist = read <$> many1 digit

ws = many (oneOf " ")
lexeme p = ws *> p <* ws

city :: Parser City
city = many1 letter

way = do
  c1 <- lexeme city
  string "to"
  c2 <- lexeme city
  return $ Way (c1, c2)

distances = do
  w <- way
  lexeme $ char '='
  d <- dist
  return (w, d)

all_dist = many (distances <* char '\n')

-- Traveling Salesman
-- Build a distance map and a city set
build_data xs = (dist_list, city_set)
  where dist_list = xs
        city_set = S.fromList (concatMap (\(Way (c1,c2), _) -> [c1, c2]) xs)

all_path city_set = permutations $ S.toList city_set

-- This function is so crappy
path_length dist_list xs = sum $ map (\x -> maybe (error "Unknown way")
                                      snd -- We return the length
                                      $ find ((== x) . fst) dist_list)
                           -- We build list of ways
                           $ zipWith mkway xs (tail xs)

solve f (dist_list, city_set) = f $ map (path_length dist_list) $ all_path city_set


main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let res = parse all_dist "" content
  let dat = build_data <$> res
  putStrLn $ "Part 1 " ++ (show $ solve minimum <$> dat)
  putStrLn $ "Part 2 " ++ (show $ solve maximum <$> dat)
