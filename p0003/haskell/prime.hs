import Data.List

factors :: Integer -> Integer -> [Integer]
factors n fac
  | fac > ((floor . sqrt) $ fromIntegral(n)) = []
  | n `mod` fac == 0 = fac:n `div` fac:next
  | otherwise = next
  where next = factors n (fac+1)

isPrime :: Integer -> Bool
isPrime n = 
  length (factors n 1) == 2
-- 600851475143 
main = do
  let ans = (head . dropWhile (not . isPrime) . reverse . sort) (factors 600851475143 1)
  putStrLn (show ans)
