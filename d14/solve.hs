module Main where
import System.Environment
import Data.List
import Control.Lens


-- I'm not sure using a record + a tuple for the state is a good way to do it?

data Reinder = Reinder {
  name :: String,
  speed :: (Int, Int),
  rest :: Int
  } deriving(Show)

data State = Running | Resting deriving(Show)

parse_line :: String -> Reinder
parse_line ln = case words ln of
  [name, "can", "fly", speed, "km/s", "for", span, "seconds,", "but", "then",
   "must", "rest", "for", rest, "seconds."] ->
    Reinder { name = name
            , speed = (read speed, read span)
            , rest = (read rest)
            }
  _ -> error $ "Bad input line: \"" ++ ln ++"\""

--                    Reinder          State          Score
prepare xs = map (\x -> (x, (Running, snd $ speed x, 0), 0)) xs

-- Maybe it's better to use the state monad
-- TODO: Try with state monad and compare

next r (Running, t, d) = if t == 1
                         then (Resting, rest r, d')
                         else (Running, t - 1, d')
  where d' = d + (fst $ speed r)
next r (Resting, t, d) = if t == 1
                         then (Running, snd $ speed r, d)
                         else (Resting, t - 1, d)

update xs = map (\x -> if x^._2._3 == max then (over (_3) (+1) x) else x) step
  where step = map (\(r, st, sc) -> (r, next r st, sc)) xs
        max = maximum $ map (^._2._3) step

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let reinders = prepare $ map parse_line $ lines content
  let states = iterate update reinders !! 2503
  putStrLn $ "Part 1:" ++ (show $ maximum $ map (^._2._3) states)
  putStrLn $ "Part 2:" ++ (show $ maximum $ map (^._3) states)
