module Main where
import System.Environment
import Data.Array.Unboxed
import Data.Foldable

type Grid = UArray (Int, Int) Bool

grid_range = ((1,1), (100, 100))

parse_input :: String -> Grid
parse_input content = array grid_range $
                      zip -- Associating index to value
                        [(x,y) | y <- [1..100], x <- [1..100]] -- Indexes
                        (map val (filter (/= '\n') content))
  where val '#' = True
        val '.' = False
        val _ = undefined


neighbors idx = filter (inRange grid_range) (surroundings idx)
  where surroundings (x,y) = [(z, w) | w <- [y-1, y+1], z <- [x-1..x+1]]
                             ++ [(x-1,y), (x+1,y)]

next :: Grid -> (Int, Int) -> Bool
next grid pos = if grid ! pos then cnt >= 2 && cnt <= 3 else cnt == 3
  where cnt = length $ filter id $ map (grid !) $ neighbors pos

in_corner idx = idx `elem` [(x,y) | x <- [1, 100], y <- [1, 100]]

next2 :: Grid -> (Int, Int) -> Bool
next2 grid pos
  | in_corner pos = True
  | otherwise = next grid pos

animate ::  (Grid -> (Int, Int) -> Bool) -> Grid -> Grid
animate nxt grid = array grid_range [((x, y), (nxt grid (x,y))) | y <- [1..100], x <- [1..100]]


solve grd nxt = length $ filter id $ elems $ (iterate (animate nxt) grd) !! 100

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let lights = parse_input content
  print $ solve lights next
  print $ solve lights next2
