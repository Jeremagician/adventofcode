module Main where
import Data.Char
import System.Environment
import Text.ParserCombinators.Parsec
import Numeric

escaped_char = (char 'x' *> (chr . fst . head . readHex <$> count 2 hexDigit))
               <|> (oneOf "\"\\")
string_char = (char '\\' *> escaped_char) <|> lower
string_literal = char '\"' *> many string_char <* char '\"'

catalog = many (string_literal <* char '\n')

diff str mem = sum $ zipWith (-) (map length str) (map length mem)

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStrLn (show (lines content))
  let res = parse catalog "" content
  putStrLn $ "Part 1: " ++ (show $ fmap (diff (lines content)) res)
  putStrLn $ "Part 2: " ++ (show $ diff (map show (lines content)) (lines content))
