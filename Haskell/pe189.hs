import qualified Data.Map.Strict as Map
import Data.Bits ((.|.), (.&.), shiftR)


red = 1
green = 2
blue = 4

genListMemo = (map genList [0..] !! )
  where genList 0 = [[]]
        genList n = [ c:l | c <- [red,green,blue], l <- genListMemo (n-1) ]

solveLevel 1 = Map.fromList [([red],1), ([green],1), ([blue],1)]
solveLevel n =
  let prevLevel = solveLevel (n-1)
  in  Map.fromList [(lcurr,
                     sum [(Map.findWithDefault 0 lprev prevLevel) * upsideDownTri lprev lcurr
                         | lprev <- genListMemo (n-1)])
                   | lcurr <- genListMemo n]
  where upsideDownTri :: [Int] -> [Int] -> Int
        upsideDownTri lprev lcurr =
          product [3 - bitsSet (c1 .|. c2 .|. c3)
                  | (c1, c2, c3) <- zip3 lprev lcurr (tail lcurr)]
        -- only applied for 0 <= num <= 7
        bitsSet num = (num .&. 1) + ((num .&. 2) `shiftR` 1) + ((num .&. 4) `shiftR` 2)

solve n = Map.foldr (+) 0 (solveLevel n)


main = do
  print $ solve 8
