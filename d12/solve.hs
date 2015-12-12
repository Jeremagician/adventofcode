module Main where
import Text.JSON
import System.Environment
import Data.Ratio
import Data.List
import Debug.Trace

extract_nums _ (JSRational _ n) = [numerator n]
extract_nums f (JSArray xs) = concatMap (extract_nums f) xs
extract_nums f (JSObject xs)  = concatMap (extract_nums f)
                                $ map snd $ f $ fromJSObject xs
extract_nums _ _ = []

is_str_eq str (JSString s) =  str == (fromJSString s)
is_str_eq _ _ = False

filter_red xs = if any ((is_str_eq "red") . snd) xs then [] else xs

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let json = decode content :: Result JSValue
  putStrLn $ "Part 1: " ++ (show $ sum . extract_nums id <$> json)
  putStrLn $ "Part 2: " ++ (show $ sum . extract_nums filter_red <$> json)
