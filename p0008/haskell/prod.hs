import Data.List

maxProd :: String -> Integer
maxProd all@(x:xs) 
  | length all < 13 = 0
  | otherwise = max (foldl (*) 1 subL) (maxProd xs)
  where
    subL = map read (words . (intersperse ' ') . (take 13) $ all) :: [Integer]

main = do
  text <- getContents
  putStrLn ((show . maxProd) text)
