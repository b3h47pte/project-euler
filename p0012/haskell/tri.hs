import Data.List

factors :: Integer -> Integer -> [Integer]
factors fac n
  | fac > ((floor . sqrt) $ fromIntegral(n)) = []
  | n `mod` fac == 0 = fac:n `div` fac:next
  | otherwise = next
  where next = factors (fac+1) n

triangleNum :: Int -> Integer
triangleNum = (map tri [1..] !!)
  where tri 1 = 1
        tri n = triangleNum (fromIntegral(n-2)) + n

triangleNums :: [Integer]
triangleNums = map triangleNum [0..]

main = do
  let ans = dropWhile ((<= 500) . length . (factors 1)) triangleNums
  putStrLn (show . head $ ans)
