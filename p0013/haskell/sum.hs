import Data.List

main = do
  contents <- getContents
  let values = (map read . lines) contents :: [Integer]
  putStrLn (take 10 . show . sum $ values)
