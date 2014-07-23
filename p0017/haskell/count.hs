import Data.List
import Data.Char

ones = ["zero", "one", "two", "three", "four", "five", "six", 
        "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", 
         "seventeen", "eighteen", "nineteen"]
ts = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

cc :: Integer -> Integer
cc n 
  | n >= 1000 = digitLength  + genericLength "thousand"
  | n >= 100 = digitLength + genericLength "hundred" + andAdd + tdAdd
  | n >= 20 = genericLength (ts !! (curDigit - 2)) + tdAdd
  | n >= 10 = genericLength (teens !! fromIntegral(restNum))
  | otherwise = genericLength (ones !! curDigit)
  where (x:xs) = show n
        curDigit = digitToInt x
        digitLength = cc . fromIntegral $ curDigit
        restNum = read xs :: Integer
        tdAdd = if restNum /= 0 then cc restNum else 0
        andAdd = if restNum /= 0 then genericLength "add" else 0

main = do
  let ans = (sum . map cc) $ [1..1000] :: Integer
  print (show ans)
