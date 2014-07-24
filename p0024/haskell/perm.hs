import Data.List

main = do
  let ans = sort . map (concat . map show) . permutations $ [0..9]
  print (show (ans !! 999999))
