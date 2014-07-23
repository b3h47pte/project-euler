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

isAmicable :: Int -> Bool
isAmicable 1 = False
isAmicable n = (pairSum == fromIntegral n) && (n /= fromIntegral pair)
  where pair = sum . allDivisors $ n
        pairSum = sum . allDivisors $ (fromIntegral pair)

main = do
  let xx = sum . filter isAmicable $ [1..9999]
  print (show xx)
