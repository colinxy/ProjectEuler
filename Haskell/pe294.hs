
ten9 = 10^9

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

choose :: Int -> Int -> Integer
choose n 0 = 1
choose n k | k > (n `div` 2) = choose n (n-k)
choose n k = (choose n (k-1)) * toInteger (n-k+1) `div` toInteger k

sum_mod xs = foldl (\acc x -> (acc+x) `mod` ten9) 0 xs

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
          in assoc_reduce recur sum_mod

-- TODO: cache this
partition_maxlen n maxlen =
  let p_lengths = partition_length_memoized n
  in sum_mod [count*((choose maxlen length) `mod` ten9) `mod` ten9
             | (length, count) <- p_lengths, length <= maxlen]

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
    rem_range i j | i == j = []
    rem_range i j | j == i+1 =
                    [(((remainders!!i) * c `mod` 23, c),
                      partition_maxlen c (maxlens!!i))
                    | c <- [0..(min (limits!!i) 23)]]
    rem_range i j =
      let rem_left = rem_range i ((i+j)`div`2)
          rem_right = rem_range ((i+j)`div`2) j
      in assoc_reduce [(((rem1+rem2)`mod`23, use1+use2), count1*count2 `mod` ten9)
                      | ((rem1, use1), count1) <- rem_left,
                        ((rem2, use2), count2) <- rem_right,
                        use1+use2 <= 23] sum_mod

solve n =
  let (maxlen, rem) = divMod n 22
      maxlens = (take rem $ repeat (maxlen+1)) ++
                (take (22-rem) $ repeat maxlen)
      limits = map (*9) maxlens
  in linear_combo remainders limits maxlens


main = do
  print $ solve 9
  print $ solve 42

  print $ solve (11^12)
