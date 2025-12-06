import Data.Char
import Data.List
import Data.List.Split

op "+" = sum
op "*" = product

calc  (sym : nums) = op sym (map read nums)

calc2 (sym : nums) = op [last sym] (map read (init sym: nums))

main = do
  ls <- return . lines =<< getContents
  print . sum . map (calc . reverse) . transpose . map words $ ls
  print . sum . map calc2 . splitWhen (all isSpace) . transpose $ ls
