import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.Array ((!))


range = 500
frogSequence = [1,1,1,1,0,0,1,1,1,0,1,1,0,1,0]

primeSet = Set.fromAscList $ takeWhile (<500) primes
  where primes = 2 : filter isPrime [3,5..]
        isPrime x = go x primes
          where go x [] = True
                go x (p:ps)
                  | p*p > x = True
                  | otherwise = x `mod` p /= 0 && go x ps

reduceFrac (a,b) = let gcdAB = gcd a b in (a`div`gcdAB, b`div`gcdAB)
addFrac (a,b) (c,d) = reduceFrac (a*d+b*c, b*d)
multFrac (a,b) (c,d) = reduceFrac (a*c, b*d)

primeFrog frogSeq =
  multFrac (1, range) $ foldr addFrac (0, 1) [frogCache ! (i, frogSeqLen) | i <- [1..range]]
  where setP num s = case (Set.member num primeSet, s) of
            (True,  1) -> (2, 3)
            (True,  0) -> (1, 3)
            (False, 1) -> (1, 3)
            (False, 0) -> (2, 3)
        frog start 1 = setP start (frogSeq' ! (frogSeqLen-1))
        frog start n
          | start == 1 = multFrac
                         (setP start (frogSeq' ! (frogSeqLen-n)))
                         (frogCache ! (start+1, n-1))
          | start == range = multFrac
                             (setP start (frogSeq' ! (frogSeqLen-n)))
                             (frogCache ! (start-1, n-1))
        frog start n =
          multFrac (setP start (frogSeq' ! (frogSeqLen-n)))
          $
          addFrac
          (multFrac (frogCache ! (start-1, n-1)) (1, 2))
          (multFrac (frogCache ! (start+1, n-1)) (1, 2))

        frogSeqLen = length frogSeq
        frogSeq' = Array.listArray (0, frogSeqLen) frogSeq
        frogCache = Array.listArray bounds
                    [frog i j | (i, j) <- Array.range bounds]
        bounds = ((1,1), (range, frogSeqLen))


main = do
  print $ primeFrog frogSequence
