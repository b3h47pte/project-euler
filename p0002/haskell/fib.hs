import Data.List

fibSeq :: (Num a, Eq a) => a -> a
fibSeq 1 = 1
fibSeq 2 = 2
fibSeq n = fibSeq (n-1) + fibSeq (n-2)

main = do
  let seq = (filter (even) . takeWhile (<=4000000) . map fibSeq) [1..]
  putStrLn (show (sum seq))
