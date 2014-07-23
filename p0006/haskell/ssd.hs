import Data.List

main = do
  (putStrLn . show) (squareOfSum - sumOfSquare)
  where squareOfSum = sum [1..100] ^ 2 
        sumOfSquare = (sum . map (^2)) [1..100]


