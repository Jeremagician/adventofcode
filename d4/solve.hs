module Main where
import Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Base16 as Hex
import Data.List
import System.Environment

md5 = C.unpack . Hex.encode . MD5.hash . C.pack
is_good_hash zc hash = and $ map (== '0') (take zc hash)
all_hashes key = map (\x -> (x, md5 $ key ++ (show x))) [0..]
find_hash zerocnt hashes = find ((is_good_hash zerocnt) . snd) hashes

main = do
  args <- getArgs
  let key = head args
  putStrLn ("Part 1: " ++ (show (find_hash 5 (all_hashes key))))
  putStrLn ("Part 2: " ++ (show (find_hash 6 (all_hashes key))))
