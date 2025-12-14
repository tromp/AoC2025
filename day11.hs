import qualified Data.Map.Strict as M

readDev s = (init dev, outs) where (dev:outs) = words s

paths outs from to = sum . map (M.findWithDefault 0 to) . takeWhile (not.null) . iterate step $ M.singleton from 1 where
  step m = M.fromListWith (+) [(o,i) | (n,i) <- M.assocs m, o <- M.findWithDefault [] n outs]

main = do
  outs <- return . M.fromList . map readDev . lines =<< getContents
  print $ paths outs "you" "out"
  print $ paths outs "svr" "fft" * paths outs "fft" "dac" * paths outs "dac" "out"
