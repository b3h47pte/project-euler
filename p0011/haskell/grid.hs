import Data.List

xLen :: [[Integer]] -> Int
xLen grid = length . head $ grid

yLen :: [[Integer]] -> Int
yLen grid = length grid

getPosition :: [[Integer]] -> (Int, Int) -> Integer
getPosition grid (x,y)
  | x < 0 || y < 0 = 0
  | x >= xLen grid || y >= yLen grid = 0
  | otherwise = (grid !! y) !! x

getDir4 :: (Int, Int) -> (Int -> Int, Int -> Int) -> [[Integer]] -> [Integer]
getDir4 (startX,startY) (dirX, dirY) grid =
  map (getPosition grid) (take 4 (iterate dirX startX `zip` iterate dirY startY))

getMaxProduct :: (Int, Int) -> [[Integer]] -> Integer
getMaxProduct (x,y) grid = 
  maximum [
    product (getDir4 (x,y) ((+) 1, id) grid),
    product (getDir4 (x,y) ((+) 1, (+) 1) grid),
    product (getDir4 (x,y) (subtract 1, (+) 1) grid),
    product (getDir4 (x,y) (subtract 1, id) grid),
    product (getDir4 (x,y) (id, (+) 1) grid),
    product (getDir4 (x,y) (id, subtract 1) grid)
  ]

solve :: [[Integer]] -> Integer
solve grid = 
  maximum prods
  where prods = [getMaxProduct (x,y) grid | x <- [0..xLen grid - 1], y <- [0..yLen grid - 1]]

main = do
  text <- getContents
  let content = map words (lines text)
  let intContent = map (map read) content :: [[Integer]]
  putStrLn (show . solve $ intContent)
