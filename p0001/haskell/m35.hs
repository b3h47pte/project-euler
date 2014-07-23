import Data.List

main = do
  let a = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]
  putStrLn (show (sum a))
