type I = (Int->Int, Int)

readRot :: String -> I
readRot (d:s) =  (if d=='R' then id else negate, read s)

rot1 :: (Int,Int) -> I -> (Int,Int)
rot1 (s,z) (f, n) = (s', z+dz) where
    s' = (s+f n) `mod` 100
    dz = if s'==0 then 1 else 0

rot2 :: (Int,Int) -> I -> (Int,Int)
rot2 (s,z) (f, n) = (s+f n, z+dz) where
    dz = (m+n) `div` 100
    m = f s `mod` 100

main = getContents >>= print . snd . foldl rot2 (50,0) . map readRot . lines
