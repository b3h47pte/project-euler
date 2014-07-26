import Data.List

fibonacci :: Int -> Integer
fibonacci = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib 2 = 1
        fib n = fibonacci (n-1) + fibonacci (n-2)

digitCount :: Integer -> Integer
digitCount n = genericLength . show $ n

main = do
  let ans = head . dropWhile ((<1000) . digitCount) . map fibonacci $ [1..]
  print (show ans)
