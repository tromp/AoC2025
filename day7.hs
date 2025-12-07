import Data.Array
import qualified Data.Map.Strict as M

n = 141  -- day7.input is n+1 x n letters

go :: Array (Int,Int) Char -> (Int->Int) -> Int -> M.Map Int Int -> Int
go ar f y ts | y==n =sum $ M.elems ts
go ar f y ts = go ar f (y+1) . M.fromListWith (+) $ do
  (t,i) <- M.assocs ts
  if ar!(y,t)=='^' then [(t-1,i),(t+1, f i)] else [(t,i)]

main = do
  ar <- return . listArray ((0,0),(n,n-1)) . filter (/= '\n') =<< getContents
  print $ go ar (const 1) 0 (M.singleton (n`div`2) 0)
  print $ go ar       id  0 (M.singleton (n`div`2) 1)
