import Data.List

-- Sieve of Eratosthenes using an arbitrary limit
primes :: [Integer]
primes = 
  2:sieves [3,5..]
  where sieves [] = [] 
        sieves (p:xs) = p : sieves [ x | x <- xs,  x `mod` p /= 0]

primeFactors :: [Integer] -> Int -> [Integer]
primeFactors allPrimes@(curPrime:primes) n = prim (fromIntegral n)
  where result n  = n `div` curPrime 
        prim 0 = [0]
        prim 1 = [1]
        prim n = if n `mod` curPrime == 0 then curPrime:ipFactors n else npFactors n
        ipFactors n = primeFactors allPrimes (fromIntegral . result $ n)
        npFactors n = primeFactors primes (fromIntegral n)

allDivisors :: Int -> [Integer]
allDivisors n = nub divs
  where pf = primeFactors primes n
        divs = filter (/= fromIntegral n) . map product $ (subsequences pf)

isAbundant :: Int -> Bool
isAbundant  = (map abu [0..] !!)
  where abu 0 = False
        abu n = (sum . allDivisors $ fromIntegral n) > n

genNonSumAbundant :: [Integer]
genNonSumAbundant =
  gen [1..1000]
  where gen [] = []
        gen (p:xs)
          | isSumOfAbundant p = gen [x | x <- xs, x `mod` p /= 0 ]
          | otherwise = fromIntegral p : gen xs
        isSumOfAbundant n = any (twoAbundant n) [1..n `div` 2]
        twoAbundant n x = isAbundant x && isAbundant (n-x)

main = do 
  print (show . sum $ genNonSumAbundant)

