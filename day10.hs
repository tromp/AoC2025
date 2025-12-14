import Data.Maybe
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split

readMach :: [String] -> (Int, [Int], [Int])
readMach (diag: bms) = (readDiag diag, map readBM (init bms), readJolt (last bms)) where
  readDiag (_:s) = sum $ zipWith light [0..] s
  light i '#' = bit i
  light i _ = 0
  readBM (_:[]) = 0
  readBM (_:d:s) = bit (digitToInt d) .|. readBM s
  readJolt ('{':s) = read ("["++init s++"]") :: [Int]

choose 0 ls = [0]
choose n [] = []
choose n (bm:ls) = (map (xor bm) $ choose (n-1) ls) ++ choose n ls

minPress (diag,bms,_) = fromJust $ find (\n -> diag `elem` choose n bms) [0..]

main = do
  machines <- return .  map (readMach . words) . lines =<< getContents
  print . sum $ map minPress machines
  putStrLn "don't want to write constraint solver or interface to one"
