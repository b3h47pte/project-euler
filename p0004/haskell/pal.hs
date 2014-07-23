import Data.List

main = do
  let ans =  maximum [x * y | x <- [100..999], y <- [100..999], isNumPalindrome (x*y)]
  putStrLn (show ans)
  where 
    stringify y = show y :: String
    isNumPalindrome x = (stringify x == (reverse . stringify) x)
