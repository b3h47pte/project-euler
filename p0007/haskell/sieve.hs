import Data.List

-- Sieve of Eratosthenes using an arbitrary limit
primes :: [Integer]
primes = 
  2:sieves [3,5..]
  where sieves [] = [] 
        sieves (p:xs) = p : sieves [ x | x <- xs,  x `mod` p /= 0]

main = do
  (putStrLn . show) (primes !! 10000)
