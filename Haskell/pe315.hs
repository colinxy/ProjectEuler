
a = 10^7
b = 2*10^7

primes = 2 : filter isPrime [3,5..b]
isPrime x = go x primes
  where go x [] = True
        go x (p:ps)
          | p*p > x = True
          | otherwise = x `mod` p /= 0 && go x ps

primesBounded n = 2 : sieve [2..n]
  where sieve [] = []
        sieve (p:xs) = p : sieve (minus xs [(p*p),(p*p+p) .. n])

        minus [] _ = []
        minus xs [] = xs
        minus (x:xs) (y:ys)
          | x == y = minus xs ys
          | x < y = x : minus xs (y:ys)
          | x > y = minus (x:xs) ys

primes7 = (takeWhile (<b) . dropWhile (<a)) (primesBounded b)

digitSum n = dsum n 0
  where dsum n accum
          | n == 0 = accum
          | otherwise = dsum (n`div`10) (accum + n`mod`10)

--   --     0
--  |  |   1 2
--   --     3
--  |  |   4 5
--   --     6
digit n
  | n == 0 = [1,1,1,0,1,1,1]
  | n == 1 = [0,0,1,0,0,1,0]
  | n == 2 = [1,0,1,1,1,0,1]
  | n == 3 = [1,0,1,1,0,1,1]
  | n == 4 = [0,1,1,1,0,1,0]
  | n == 5 = [1,1,0,1,0,1,1]
  | n == 6 = [1,1,0,1,1,1,1]
  | n == 7 = [1,1,1,0,0,1,0]
  | n == 8 = [1,1,1,1,1,1,1]
  | n == 9 = [1,1,1,1,0,1,1]

displayDigit n = sum $ digit n

displayNumber n = dispNum n 0
  where dispNum 0 accum = accum
        dispNum n accum = dispNum (n`div`10) (accum + displayDigit (n`mod`10))

changeDigit m n | m == n = 0
changeDigit m n = sum $ map (\(a,b) -> abs (a-b)) $ zip (digit m) (digit n)

changeNumber m 0 = displayNumber m
changeNumber 0 n = displayNumber n
changeNumber m n = chNum m n 0
  where chNum 0 0 accum = accum
        chNum m 0 accum = accum + displayNumber m
        chNum 0 n accum = accum + displayNumber n
        chNum m n accum = chNum (m`div`10) (n`div`10)
                          (accum + changeDigit (m`mod`10) (n`mod`10))

samClock n = clock n 0
  where clock n accum | n < 10 = (accum + 2 * displayNumber n)
        clock n accum = clock (digitSum n) (accum + 2 * displayNumber n)

maxClock n = displayNumber n + clock n 0
  where clock n accum | n < 10 = displayNumber n + accum
        clock n accum = let dsum = digitSum n
                        in clock dsum (accum + changeNumber n dsum)

solve numbers = sum [samClock n - maxClock n | n <- numbers]


main = do
  -- mojority of time used to generate primes
  -- print $ length primes7
  print $ solve primes7
