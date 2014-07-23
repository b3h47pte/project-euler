import Data.List

collatz :: Integer -> [Integer]
collatz n
  | n == 1 = [1]
  | even n = n `div` 2:collatz (n `div` 2)
  | otherwise = 3 * n + 1:collatz (3 * n + 1)

maxCompare :: (Integer, Int) -> (Integer, Int) -> (Integer, Int)
maxCompare (x,y) (x',y')
    | y > y' = (x,y)
    | otherwise = (x', y')

main = do
  let chain = [(x,y) | x <- [1..999999],
          let y = length . collatz $ x]
  let ans = foldl' maxCompare (0,0) chain
  putStrLn (show ans)
