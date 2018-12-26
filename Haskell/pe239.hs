
primes = 25
foolishPrimes = 22

factorial 0 = 1
factorial n = n * factorial (n-1)

choose n 0 = 1
choose n k | k > (n `div` 2) = choose n (n-k)
choose n k = (choose n (k-1)) * (n-k+1) `div` k

-- partial derangement
-- derangement = total - arrangement
-- arrangement = \sum_{k=1}^{m} -(-1)^k (foolishPrimes choose k) (n-k)!
derangementPartial n m = factorial n - arrangementPartial n m 1 0
  where arrangementPartial n m k accum | m < k = accum
        arrangementPartial n m k accum =
          arrangementPartial n m (k+1)
          (accum - (-1)^k * (factorial (n-k)) * (choose foolishPrimes k))

solve n =
  let chooseFoolishPrimes = choose primes foolishPrimes
      derange = derangementPartial (n - (primes-foolishPrimes)) foolishPrimes
  in fromIntegral (chooseFoolishPrimes * derange) / fromIntegral (factorial n)

main = do
  print $ solve 100
