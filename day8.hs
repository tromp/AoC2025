import Data.Maybe
import Data.Array
import Data.List
import Data.List.Split

dist2 (x0,y0,z0) (x1,y1,z1) = (x1-x0)^2+(y1-y0)^2+(z1-z0)^2

readD3 :: String -> (Int, Int, Int)
readD3 s = (x,y,z) where [x,y,z] = map read $ splitOn "," s

equiv (ar,_) (b0,b1) = if r0==r1 then (ar,-ar!r0) else (ar//dlt,-negsize) where
  dlt = if ar!r0 < ar!r1 then [(r1,r0), (r0,negsize)] else [(r0,r1), (r1,negsize)]
  negsize = ar!r0 + ar!r1
  (r0,r1) = (uf b0, uf b1)
  uf i = if ar!i < 0 then i else uf (ar!i)

main = do
  boxes <- return . zip [0..] . map readD3 . lines =<< getContents
  let n = length boxes
  let distIds = sort [(dist2 (snd b0) (snd b1), (fst b0,fst b1)) | (b0:bs)<-tails boxes, b1<-bs]
  let circs = scanl' equiv (listArray (0,n-1) (repeat (-1)), 0) $ map snd distIds
  print . product . take 3 . map negate . sort . elems . fst $ circs!!n
  let (_,(i0,i1)) = (distIds!!) . pred . fromJust $ findIndex (\(_,s) -> s==n) circs
  let ((_,(x0,_,_)),(_,(x1,_,_))) = (boxes!!i0, boxes!!i1)
  print $ x0 * x1
