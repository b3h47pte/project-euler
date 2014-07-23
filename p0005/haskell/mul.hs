import Data.List

mlcm :: Integer -> Integer -> Integer
mlcm x y = abs (x*y) `div` gcd x y

main = do
  let ans = foldl mlcm 1 [1..20]
  putStrLn (show ans)
