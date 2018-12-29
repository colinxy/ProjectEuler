
ten9 = 10^9

-- taking mod is safe because
-- 2^(x+10^9) = 2^x mod 10^9 for x >= 9
-- observation by @neverforget https://projecteuler.net/thread=396#82384
powMod n k = pow n k 1
  where pow n 0 accum = accum
        pow n k accum
          | k `mod` 2 == 0 = pow (n*n `mod` ten9) (k`div`2) accum
          | otherwise = pow n (k-1) (accum*n `mod` ten9)

-- how many rounds does it take for k^nth to go to 0 (starting at base k)
-- 1 -> 0
wgSub k 0 = k+1
-- 1 0 (base k) -> 0 k (base k+1)
-- wgSub k 1 = loopWgSub k (k+1) 0
wgSub k 1 = (2*k+1) `mod` ten9
-- 1 0 0 (base k) -> 0 k k (base k+1)
-- wgSub k 2 = loopWgSub k (loopWgSub k (k+1) 0) 1
-- wgSub k 2 = loopWgSub (k+1) k 1
wgSub k 2 = (k * powMod 2 (k+1) + (powMod 2 (k+1) - 1)) `mod` ten9
wgSub k nth = loopWgSub (k+1) k (nth-1)

loopWgSub 0 k nth = k
loopWgSub times k 0 = (k + times) `mod` ten9
-- k * 2^k + 2^k-1
loopWgSub times k 1 = (k * powMod 2 times + (powMod 2 times - 1)) `mod` ten9
loopWgSub times k nth = loopWgSub (times-1) (wgSub k nth) nth

wGoodstein n = go n 2 0 - 2
  where go 0 k nth = k
        go n k nth = go (n`div`2) (loopWgSub (n`mod`2) k nth) (nth+1)


main = do
  -- print $ wGoodstein 6
  print $ sum (map wGoodstein [1..15]) `mod` ten9
