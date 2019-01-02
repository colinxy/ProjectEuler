import Data.Array (range)


-- (1 + ... + m) * (1 + ... + n)
regular m n = (m * (m+1) `div` 2) * (n * (n+1) `div` 2)

maxDiag m n width
  | odd width && m == n = (m*2-1 - width, 2)
  | odd width && m /= n = ((min m n)*2 - width, abs (m-n))
  | otherwise = ((min m n)*2 - width, abs (m-n) + 1)

tiltedSideLength m n side = go 2 0
  where (mDiag, count) = maxDiag m n side
        -- (1 + 2) * 2 +
        -- (1 + ... + 4) * 2 +
        -- (1 + ... + mDiag) * count
        go diag accum
          | diag < mDiag = go (diag+2) (accum + diag*(diag+1))
          | otherwise = accum + mDiag*(mDiag+1)`div`2 * count

tilted m n = go 1 0
  where (maxSideLength, _) = maxDiag m n 1
        go side accum
          | side > maxSideLength = accum
          | otherwise = go (side+1) (accum + tiltedSideLength m n side)

crossHatched m n = regular m n + tilted m n

crossHatchedCumulative m n = sum [crossHatched i j
                                 | (i, j) <- range ((1,1), (m,n))]


main = do
  print $ crossHatchedCumulative 47 43
