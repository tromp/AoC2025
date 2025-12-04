import Data.List
import Data.Array
import Data.Ix

type XY = (Int,Int)
type A = Array XY Char

n = 135  -- day4.input is n x n letters
paperBounds = ((0,0),(n-1,n-1))

nbrs :: A -> XY -> [Char]
nbrs ar (x,y) = [ar!(x',y') | x'<-[x-1..x+1], y'<-[y-1..y+1], (x',y')/=(x,y), inRange paperBounds (x',y')]

accessible :: A -> XY -> Bool
accessible ar (x,y) = length (filter (=='@') (nbrs ar (x,y))) < 4

removable1 :: A -> [XY]
removable1 ar = [(x,y) | x<-[0..n-1], y<-[0..n-1], ar!(x,y)=='@', accessible ar (x,y)]

removable :: A -> Int
removable ar = case removable1 ar of
  [] -> 0
  rm -> length rm + removable (ar // [(xy,'.') | xy <- rm])

main = do
  ar <- return . listArray paperBounds . filter (/= '\n') =<< getContents
  print . length . removable1 $ ar
  print . removable $ ar
