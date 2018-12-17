
-- took me so long to find out: Int overflow
remainders = map (\x -> fromInteger (((10^x-1) :: Integer) `mod` 23)) [0..21]

rmdups eq [] = []
rmdups eq [x] = [x]
rmdups eq (x:xs) = x : (rmdups eq $ filter (not . (eq x)) xs)

assoc_reduce kv_pairs reducer =
  [(key, reducer [value | (k, value) <- kv_pairs, k == key])
  | key <- rmdups (==) (map fst kv_pairs)]

assoc_get kv_pairs key =
  snd . head $ filter ((==key) . fst) kv_pairs

-- n choose k mod (10^9)
ten9 = 10^9 :: Int
choose_mod :: Int -> Int -> Int
choose_mod n 0 = 1
choose_mod n k | k > (n `div` 2) = choose_mod n (n-k)
choose_mod n k =
  choose_mod' n k 1 0 0
  where choose_mod' :: Int -> Int -> Int -> Int -> Int -> Int
        choose_mod' n 0 accum twos fives =
          fromInteger ((toInteger accum)
                       * (2^(toInteger twos) `mod` (toInteger ten9))
                       * (5^(toInteger fives) `mod` (toInteger ten9))
                       `mod` (toInteger ten9))
        choose_mod' n k' accum twos fives =
          let (n2, n5, n_left) = strip2s5s n 0 0
              (k2, k5, k_left) = strip2s5s (k-k'+1) 0 0
          in choose_mod' (n-1) (k'-1) (accum*n_left`div`k_left `mod` ten9)
             (twos+n2-k2) (fives+n5-k5)
        strip2s5s n twos fives | n`mod`2 == 0 = strip2s5s (n`div`2) (twos+1) fives
        strip2s5s n twos fives | n`mod`5 == 0 = strip2s5s (n`div`5) twos (fives+1)
        strip2s5s n twos fives = (twos, fives, n)

-- partition n with 1..9
-- not real partition (allow permutation)
partition_memoized = (map partition [0..] !!)
  where partition 0 = 1
        partition n = sum [partition_memoized (n-i) | i <- [1..9], n-i>=0]

partition_length_memoized = (map partition [0..] !!)
  where partition 0 = [(0, 1)]
        partition n =
          let recur = [(length+1, count) | i <- [1..9], n-i>=0,
                       (length, count) <- partition_length_memoized (n-i)]
          in assoc_reduce recur sum

partition_maxlen n maxlen =
  let p_lengths = partition_length_memoized n
  in (sum [count*(choose_mod maxlen length)
          | (length, count) <- p_lengths, length <= maxlen]) `mod` ten9

-- remainders R_i
-- \sum {a_i * R_i} mod 23 = 0
-- \sum {a_i} = 23
-- a_i < u_i
-- count number of such a_i
linear_combo_slow remainders limits =
  count_it remainders limits 0 23
  where
    count_it remainders [] 0 0 = 1
    count_it remainders [] _ _ = 1
    count_it (rem:remainders) (u:limits) s left =
      sum [count_it remainders limits ((s + rem*use) `mod` 23) (left-use)
          | use <- [0..(min u left)]]

linear_combo remainders limits maxlens =
  let rems_uses = rem_range 0 (length limits)
  in assoc_get rems_uses (0, 23)
  where
    rem_range i j | j == i+1 =
                    [((remainders!!i * c `mod` 23, c),
                      partition_maxlen c (maxlens!!i))
                    | c <- [0..(min (limits!!i) 23)]]
    rem_range i j =
      let rem_left = rem_range i ((i+j)`div`2)
          rem_right = rem_range ((i+j)`div`2) j
      in assoc_reduce [(((rem1+rem2)`mod`23, use1+use2), count1*count2 `mod` ten9)
                      | ((rem1, use1), count1) <- rem_left,
                        ((rem2, use2), count2) <- rem_right,
                        use1+use2 <= 23] (foldl (\acc x -> (acc+x)`mod`ten9) 0)

solve n =
  let (maxlen, rem) = divMod n 22
      maxlens = (take rem $ repeat (maxlen+1)) ++
                (take (22-rem) $ repeat maxlen)
      limits = map (*9) maxlens
  in linear_combo remainders limits maxlens


main = do
  print $ solve 9
  print $ solve 42

  putStrLn $ "Correct   S(792): " ++ (show $ solve 792)
  putStrLn $ "Incorrect S(793): " ++ (show $ solve 793)
  -- print $ solve (11^12)
