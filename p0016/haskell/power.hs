import Data.List
import Data.Char

main = do
  let num = show (2 ^ 1000) :: String
  print (show . sum . (map digitToInt) $ num) 
