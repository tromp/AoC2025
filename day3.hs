import Data.List
import Data.Char

jolt :: Int -> [Int] -> Int
jolt n s = go sn s' where
  (sn,s') = splitAt n s
  go ds [] = foldl' (\a b-> 10*a + b) 0 ds
  go ds@(_:ds1) (c:cs) = go (map fst dGE ++ post) cs where
    (dGE,dL) = span (uncurry (>=)) (zip ds ds1)
    post = if null dL then [max (last ds1) c] else map snd dL ++ [c]

main = do
  banks <- return . map (map digitToInt) . lines =<< getContents
  print . sum . map (jolt  2) $ banks
  print . sum . map (jolt 12) $ banks
