type Rot = (Int->Int, Int)
type State = (Int, Int, Int)

readRot :: String -> Rot
readRot (d:s) =  (if d=='R' then id else negate, read s)

go :: State -> Rot -> State
go (dial,z1,z2) (f, n) = (dial', z1+dz1, z2+dz2) where
    dial' = (dial + f n) `mod` 100
    dz1 = if dial' == 0 then 1 else 0
    dz2 = ((f dial `mod` 100) + n) `div` 100

main = do
  (_,z1,z2) <- return . foldl go (50,0,0) . map readRot . lines =<< getContents
  print z1
  print z2
