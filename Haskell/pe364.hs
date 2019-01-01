import qualified Data.Array as Array
import Data.Array ((!))
import qualified Data.Map.Strict as Map

-- Stage 1: take all seats where adjacent seat(s) are not occupied
-- _X__X_X_X
-- empty seats have run of length 1 or 2
-- Stage 2: take all seats where only one adjacent seat is occupied
-- both ends and empty seat run of length 2
-- Stage 3: take remaining seats

modu = 100000007
maxSize = 10^6

powMod :: Int -> Int -> Int
powMod n k = go n k 1
  where go n 0 accum = accum
        go n k accum
          | k `mod` 2 == 1 = go n (k-1) (accum*n `mod` modu)
          | otherwise = go (n*n `mod` modu) (k`div`2) accum

modInv n = powMod n (modu-2)

factorialMod n = factorialModCache ! n
factorialModCache = Array.listArray (0,maxSize)
                    [factMod i | i <- Array.range (0,maxSize)]
  where factMod 0 = 1
        factMod n = n * factorialMod (n-1) `mod` modu

-- modu is large prime
chooseMod n k
  | k*2 > n = chooseMod n (n-k)
  | otherwise = (factorialMod n) *
                (modInv ((factorialMod (n-k) * factorialMod k) `mod` modu))
                `mod` modu

-- -- NOT USED
-- -- SLOW FIRST VERSION
-- --
-- endsAt n =
--   let (endsAtN, endsAtN_1) =
--         case n of
--           2 -> (endsAtRight2, endsAtRight1)
--           3 -> (endsAtRight3, endsAtRight2)
--           4 -> (endsAtRight4, endsAtRight3)
--           _ -> go 5 endsAtRight4 endsAtRight3 endsAtRight2
--   in Map.unionWith (\a b -> (a+b) `mod` modu) endsAtN
--      (Map.mapKeys (\(ends, run1, run2) -> (ends-1, run1, run2)) endsAtN_1)
--   where
--     -- right end must be occupied
--     go curr endsAtRightN_1 endsAtRightN_2 endsAtRightN_3
--       | curr-1 == n = (endsAtRightN_1, endsAtRightN_2)
--       | otherwise =
--         let endsAtRightN =
--               Map.unionWith (\a b -> (a+b) `mod` modu)
--               -- ---X_X
--               (Map.mapKeysMonotonic (\(ends, run1, run2) -> (ends, run1+1, run2)) endsAtRightN_2)
--               -- ---X__X
--               (Map.mapKeysMonotonic (\(ends, run1, run2) -> (ends, run1, run2+1)) endsAtRightN_3)
--         in go (curr+1) endsAtRightN endsAtRightN_1 endsAtRightN_2
--     -- right end must be occupied
--     endsAtRight1 = Map.fromList [((1, 0, 0), 1)]
--     endsAtRight2 = Map.fromList [((1, 0, 0), 1)]
--     endsAtRight3 = Map.fromList [((2, 1, 0), 1)]
--     endsAtRight4 = Map.fromList [((1, 1, 0), 1), -- _X_X
--                                  ((2, 0, 1), 1)] -- X__X

-- solve n =
--   let endsAtN = endsAtGen n
--   in Map.foldrWithKey
--      (\(ends, run1, run2) count accum ->
--          (accum + count * stage1 ends run1 run2) `mod` modu)
--      0 endsAtN


-- ends (= n-1 - run1*2 - run2*3): is left/right end occupied (0, 1, 2)
-- run1: number of empty seat runs of length 1
-- run2: number of empty seat runs of length 2
-- taken (= run1+run2+1): number of seats taken at the end of stage 1
stage1 ends run1 run2 = (factorialMod (run1+run2+1) * stage2 ends run1 run2) `mod` modu
stage2 ends run1 run2 = ((powMod 2 run2 * factorialMod ((2-ends)+run2)) `mod` modu) * stage3 (run1+run2) `mod` modu
stage3 remain = factorialMod remain

-- n-1 = (2-ends) + run1*2 + run2*3
solve n =
  foldr (\a b -> (a+b) `mod` modu) 0
  [(stage1 ends run1 run2) *
   (chooseMod (run1+run2) run1) *
   (if ends == 1 then 2 else 1) `mod` modu
  | run2 <- [0..((n-1) `div` 3)],
    run1 <- let left = n-1 - run2*3
            in if left `mod` 2 == 0 && left `div` 2 > 0
               then [left `div` 2, left `div` 2 - 1]
               else [left `div` 2],
    ends <- [2 - (n-1 - run1*2 - run2*3)]]


main = do
  print $ solve 10
  print $ solve 1000
  print $ solve 1000000
