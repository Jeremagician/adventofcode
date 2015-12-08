module Main where
import Data.Word
import Data.Bits
import System.Environment
import Data.List
import Data.List.Utils
import Text.ParserCombinators.Parsec

import Debug.Trace

data Signal = Val Word16
            | Wire String
            | Unop (Word16 -> Word16) Signal
            | Binop Signal (Word16 -> Word16 -> Word16) Signal

instance Show Signal where
  show (Val v) = show v
  show (Wire w) = w
  show (Unop _ x) = "UNOP " ++ show x
  show (Binop x _ y) = show x ++ " BINOP " ++ show y

-- Parsing stuff
int :: Parser Word16
int = read <$> many1 digit

str = many1 lower

ws = many (oneOf " ")
lexeme p = ws *> p <* ws

unop = (string "NOT" *> return complement)

sl x y = shiftL x (fromIntegral y)
sr x y = shiftR x (fromIntegral y)

binop =  (string "OR" *> return (.|.))
         <|> (string "AND" *> return (.&.))
         <|> (string "LSHIFT" *> return sl)
         <|> (string "RSHIFT" *> return sr)

atom = try (Val <$> int)
       <|> (Wire <$> str)

signal = try (Binop <$> atom <*> lexeme binop <*> atom)
         <|> (Unop <$> lexeme unop <*> atom)
         <|> atom

inst = do
  sig <- signal
  lexeme $ string "->"
  wire <- str
  return (wire, sig)

booklet = many (inst <* char '\n')

-- Circuit Assembly
solve wire booklet = solve' (Wire wire)
  where solve' (Val v) = Just v
        solve' (Wire w) =  find ((== w) . fst) booklet >>= (solve' . snd)
        solve' (Unop f x)  = f <$> (solve' x)
        solve' (Binop x f y)  =  f <$> (solve' x) <*> (solve' y)

table booklet =  [x | x@(_, Val _) <- booklet]

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let bklt = parse booklet "" content
  putStrLn (show $ fmap table bklt)
