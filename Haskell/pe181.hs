import qualified Data.Array as Array
import Data.Array ((!))


black = 60
white = 40
limit = max black white

-- partition
partition 0 = 1
partition n = pCache ! (1, n)
pFrom from 0 = 1
pFrom from n
  | from > n = 0
  | from == n = 1
  | otherwise = pCache ! (from, n-from) + pCache ! (from+1, n)
pCache = Array.listArray ((1,1), (limit,limit))
         [pFrom i j | (i, j) <- Array.range ((1,1), (limit,limit))]

-- partition a pair of 2 numbers (no 0 in pairs)
partition2 n1 n2 = p2Cache ! (1, n1, 1, n2)
p2From _ 0 _ 0 = 1
p2From from1 n1 from2 n2
  | from1 > n1 = 0
  | from1 == n1 = if from2 <= n2 then 1 else 0
  | from2 >= n2 = p2Cache ! ((from1+1), n1, 1, n2)
  | otherwise = p2Cache ! (from1, (n1-from1), from2, (n2-from2)) +
                p2Cache ! (from1, n1, (from2+1), n2)
p2Cache = Array.listArray bounds
          [p2From i j k l | (i, j, k, l) <- Array.range bounds]
  where bounds = ((1,1,1,1), ((black,black,white,white)))

partitionGroup black white =
  (partition black * partition white) +
  sum [(partition (black-b)) * (partition (white-w)) * (partition2 b w)
      | (b, w) <- Array.range ((1,1), (black,white))]


main = do
  print $ partitionGroup black white
