import Data.List
import Data.Array
import Control.Applicative

lattice :: (Integer, Integer) -> Integer
lattice (0,0) = 1
lattice (-1,_) = 0
lattice (_,-1) = 0
lattice (x, y) = lattice (x - 1, y) + lattice(x, y - 1)

maxX = 20
maxY = 20

lattice' :: (Integer, Integer) -> Integer
lattice' = (listArray ((0,0),(maxX,maxY)) dat `ele`)
  where dat = [val | x <- [0..maxX], y <- [0..maxY], let val = calcVal x y]
        calcVal 0 0 = 1 
        calcVal (-1) _ = 0
        calcVal _ (-1) = 0 
        calcVal x y = lattice' (x - 1, y) + lattice' (x, y - 1)

ele arr (x,y)
  | x < 0 || y < 0 = 0
  | x > maxX || y > maxY = 0
  | otherwise = arr ! (x,y)

main = 
  print (show .  lattice' $ (20, 20))
