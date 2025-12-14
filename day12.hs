import Data.List.Split

readRegion :: String -> (Int, [Int])
readRegion s = (x*y, map read cnts) where
  (xy:cnts) = words s
  [x,y] = map read . splitOn "x" $ init xy

readShape = sum . map (length . filter (== '#'))

fits shps (sz, ns) = sz >= sum (zipWith (*) shps ns)

main = do
  shapesRegions <- return . splitOn [""] . lines =<< getContents
  let shapes  = map readShape  $ init shapesRegions
  let regions = map readRegion $ last shapesRegions
  print . length . filter (fits shapes) $ regions
