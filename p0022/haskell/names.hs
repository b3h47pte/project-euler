import Data.List
import Data.Char

getNameScore :: String -> Integer
getNameScore name = (fromIntegral . sum . map (ord . toUpper) $ name) - fromIntegral offset
  where offset = length name * (ord 'A' - 1)

splitNames :: String -> [String]
splitNames names = words . map cmToSpace . filter (/= '"') $ names
  where cmToSpace ',' = ' '
        cmToSpace x = x

main = do
  contents <- getContents
  let dat = map getNameScore . sort . splitNames $ contents
  let res = sum . zipWith (*) dat $ [1..]
  print (show res)
