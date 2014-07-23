import Data.List

data BinaryTree = Empty | Node Integer BinaryTree BinaryTree

createBinaryTree :: (Integer, Integer) -> [[Integer]] -> BinaryTree
createBinaryTree (x, y) dat
  | x > y = Empty
  | y >= genericLength dat = Empty
  | otherwise = Node (dat !! fromIntegral y !! fromIntegral x) left right
  where left = createBinaryTree (x, y + 1) dat
        right = createBinaryTree(x + 1, y + 1) dat

maximumPath :: BinaryTree -> [Integer]
maximumPath Empty = []
maximumPath (Node val left right) 
  | sumL >= sumR = val:leftL
  | otherwise = val:rightL
  where leftL = maximumPath left
        rightL = maximumPath right 
        sumL = sum leftL
        sumR = sum rightL

main = do
  contents <- getContents
  let tree = createBinaryTree (0,0) . (map $ map read) . map words . lines $ contents
  print (show . sum . maximumPath $ tree)
