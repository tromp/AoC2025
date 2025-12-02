import Data.List
import Data.List.Split

invalid :: Int -> Int -> Bool
invalid rpt n = or [drop l sn `isPrefixOf` sn | l<-[1..lsn-1], lsn`mod`l==0, lsn`div`l <= rpt] where
  sn = show n
  lsn = length sn

doRange rpt s = let sl@[s0,s1] = splitOn "-" s
  in sum [if invalid rpt n then n else 0 | n<-[read s0..read s1]]

main = do
  ranges <- return . splitOn "," =<< getContents
  print . sum . map (doRange 2) $ ranges 
  print . sum . map (doRange maxBound) $ ranges 
