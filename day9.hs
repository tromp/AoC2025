import Data.Maybe
import Data.List
import Data.List.Split

readXY :: String -> (Int, Int)
readXY s = (x,y) where [x,y] = map read $ splitOn "," s

outside ((x0,y0),(x1,y1)) (x,y) = (x-x0)*(x-x1)>=0 || (y-y0)*(y-y1)>=0

main = do
  reds <- return . map readXY . lines =<< getContents
  let rects = reverse $ sort [((abs(x1-x0)+1)*(abs(y1-y0)+1),(r0,r1))
                             | r0@(x0,y0):rs <- tails reds, r1@(x1,y1)<-rs]
  print . fst $ head rects
  let mids = zipWith (\(x0,y0) (x1,y1) -> ((x0+x1)`div`2,(y0+y1)`div`2)) reds (last reds: reds)
  print . fst . fromJust $ find (\(_,rect) -> all (outside rect) (reds++mids)) rects
