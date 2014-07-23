import Data.List
import Data.Char

factorial :: Int -> Integer
factorial = (map fac [0..] !!)
  where fac 0 = 0
        fac 1 = 1
        fac n = n * factorial ((fromIntegral n) - 1)

sumDigits :: Integer -> Integer
sumDigits n = sum digits
  where strNum = show n :: String
        digits = map (fromIntegral . digitToInt) strNum

main = print (show . sumDigits . factorial $ 100)

