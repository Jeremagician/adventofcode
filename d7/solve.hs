module Main where
import Data.Word
import Data.Bits
import System.Environment
import Data.List
import Data.List.Utils
import Text.ParserCombinators.Parsec

data Signal = Val Word16
            | Wire String
            | Unop (Word16 -> Word16) Signal
            | Binop Signal (Word16 -> Word16 -> Word16) Signal

instance Show Signal where
  show (Val v) = show v
  show (Wire w) = w
  show (Unop _ x) = "UNOP " ++ show x
  show (Binop x _ y) = show x ++ " BINOP " ++ show y

instance Eq Signal where
  Val x == Val y = x == y
  Wire x == Wire y = x == y
  Unop f x == Unop g y = x == y
  Binop x f y == Binop z g w = x == z && y ==w
  _ == _ = False

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

-- Top down method, don't work, don't know why
-- I'll figure it out ... eventually
solve wire booklet = solve' (Wire wire)
  where solve' (Val v) = Just v
        solve' (Wire w) =  find ((== w) . fst) booklet >>= (solve' . snd)
        solve' (Unop f x)  = f <$> (solve' x)
        solve' (Binop x f y)  =  f <$> (solve' x) <*> (solve' y)

-- Bottom up method
apply repl (Wire x) = let xs = filter ((== x) . fst) repl
                       in if (length xs > 0)
                          then snd (head xs)
                          else Wire x
apply repl (Unop f (Val x)) = Val (f x)
apply repl (Unop f x) = Unop (f) (apply repl x)
apply repl (Binop (Val x) f (Val y)) = Val (f x y)
apply repl (Binop x f y) = Binop (apply repl x) (f) (apply repl y)
apply repl x = x

table :: [(String, Signal)] -> [(String, Signal)]
table booklet = build [] booklet
  where build last bk = let repl = [x | x@(_, Val _) <- bk]
                        in if (length repl) == (length bk)
                           then bk
                           else build repl (map (fmap (apply repl)) bk)

main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let bklt = parse booklet "" content
  putStrLn (show $ fmap (find ((== (args !! 1)) . fst) . table) bklt)
