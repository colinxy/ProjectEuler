# Consider only letter A and B in a word: they are formed by
# interleaving runs of As and Bs. For example,
#
#   AA BB A BBB
#
# Let there be k_A runs of A, and k_B runs of B, then k_A and k_B
# differ by no more than 1. The above example has k_A = 2 runs of A
# and k_B = 2 runs of B. We can count the number of ways to split As
# and Bs into runs using
# https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)
# where no empty bins are allowed.
#
# Since there must be at least 2 Cs between runs of As and Bs, there
# must be at least 2(k_A+k_B-1) Cs between runs of runs of As and
# Bs. Ignore 2 Cs between every run of As and Bs, and the remaining Cs
# can be placed anywhere in between As and Bs. This is solvable by the
# same combinatorial problem except that empty bins are allowed.
#
# Also consider problem 862, which also uses stars and bars.

from math import comb

from mathutil import extended_gcd, product_mod

# this is a prime
MOD = 10**9+7


def W(p, q, r, comb=comb, mod=lambda x: x):
    total = 0

    for runs_A in range(1, p+1):
        runs_Bs = []
        if runs_A-1 > 0:
            runs_Bs.append(runs_A-1)
        if runs_A <= q:
            runs_Bs.append(runs_A)
        if runs_A+1 <= q:
            runs_Bs.append(runs_A+1)

        for runs_B in runs_Bs:
            remain_C = r - 2*(runs_A+runs_B-1)
            if remain_C < 0:
                continue

            count_AB = mod(comb(p-1, runs_A-1) * comb(q-1, runs_B-1))
            if runs_A == runs_B:
                count_AB *= 2
            count_ABC = mod(count_AB * comb(p+q+remain_C, p+q))
            total = mod(total + count_ABC)

    return total


_mod_inverse_cache = {}

def mod_inverse(n):
    if res := _mod_inverse_cache.get(n, None):
        return res

    inv, _ = extended_gcd(n, MOD)
    # inv might be negative, so take another modular to ensure it's positive
    inv = inv % MOD

    _mod_inverse_cache[n] = inv
    # _mod_inverse_cache[inv] = n
    return inv


def comb_mod_cache_n(n, k):
    cache = [1]
    c = 1
    for i in range(1, k+1):
        c = c * n * mod_inverse(i) % MOD
        cache.append(c)
        n -= 1
    return cache


def comb_mod_cache_k(k, n0, n1, gap):
    cache = []

    factorial_k = product_mod(range(1, k+1), MOD)
    factorial_k_mod = mod_inverse(factorial_k)

    prod_n = product_mod(range(n0, n0-k, -1), MOD)
    comb_n_k_mod = prod_n * factorial_k_mod % MOD
    cache.append(comb_n_k_mod)

    for n in range(n0+gap, n1+gap, gap):
        # n, n-1, ..., n-k+1
        # n-gap, ..., n-gap-k+1
        n_extend = product_mod(range(n, n-gap, -1), MOD)
        n_shrink = product_mod(range(n-gap-k+1, n-k+1), MOD)

        prod_n = prod_n * n_extend * mod_inverse(n_shrink) % MOD
        comb_n_k_mod = prod_n * factorial_k_mod % MOD
        cache.append(comb_n_k_mod)

    return cache


def W_cached(p, q, r):
    # assume:
    #   p <= q
    #   2*(max_runs-1) <= r
    comb_p_1 = comb_mod_cache_n(p-1, p-1)
    comb_q_1 = comb_mod_cache_n(q-1, min(p, q-1))

    min_runs = 2
    max_runs = min(2*p+1, p+q)
    n0 = p+q+r - 2*(max_runs-1)
    n1 = p+q+r - 2
    gap = 2
    comb_k_pq = comb_mod_cache_k(p+q, n0, n1, gap)

    def comb(n, k):
        if n == p-1:
            return comb_p_1[k]
        if n == q-1:
            return comb_q_1[k]
        if k == p+q:
            return comb_k_pq[(n-n0)//gap]

    def mod(n):
        return n % MOD

    return W(p, q, r, comb, mod)


if __name__ == '__main__':
    print(W(2, 2, 4))
    print(W(4, 4, 44))

    print(W_cached(10**6, 10**7, 10**8))
