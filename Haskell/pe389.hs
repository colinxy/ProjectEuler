
-- Z = \sum_{i=1}^{N}{X_i}
-- var(Z) = var(X_i) E[N] + E[X_i]^2 var(N)

-- n sided dice
-- E[X]
e n = (1+n) / 2
-- E[X^2]
e2 n = (sum $ map (^2) [1..n]) / n
-- var(X)
var n = (e2 n) - (e n) ^ 2

-- 4 sided
-- E[T]
e_T = e 4
-- var(T)
var_T = var 4

-- 6 sided
-- E[C]
e_C = e_T * (e 6)
-- var(C)
var_C = (var 6) * e_T + (e 6) ^ 2 * var_T

-- 8 sided
-- E[O]
e_O = e_C * (e 8)
-- var(O)
var_O = (var 8) * e_C + (e 8) ^ 2 * var_C

-- 12 sided
-- E[D]
e_D = e_O * (e 12)
-- var(D)
var_D = (var 12) * e_O + (e 12) ^ 2 * var_O

-- 20 sided
-- E[I]
e_I = e_D * (e 20)
-- var(I)
var_I = (var 20) * e_D + (e 20) ^ 2 * var_D


main = do
  print var_I
