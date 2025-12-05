import Data.List.Split

readRng s = (a,b) where [a,b] = map read . splitOn "-" $ s

isFresh rngs i = any (\(a,b) -> a <= i && i <= b) rngs

merge :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
merge rng [] = [rng]
merge r0@(_,r) rs@(r1@(l,_):_) | r < l = r0 : rs
merge r0@(l,_)   (r1@(_,r):rs) | l > r = r1 : merge r0 rs
merge (l0,r0) ((l1,r1):rs) = merge (min l0 l1, max r0 r1) rs

main = do
  [fresh_,ingr_] <- return . splitOn [""] . lines =<< getContents
  let fresh = map readRng fresh_
  let ingr = map read ingr_
  print . length . filter (isFresh fresh) $ ingr
  print . sum . map (\(a,b) -> b+1-a) . foldr merge [] $ fresh
