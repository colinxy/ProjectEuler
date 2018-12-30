# Writing 1/2 as a sum of inverse squares

from fractions import Fraction as F
from math import log
import sys

from mathutil import prime_under, extended_gcd, prime_factorization


def decompose(n):
    """Decompose inverse square as sum of prime inverse squares"""
    p_fact = sorted(prime_factorization(n).items())
    facts_exp = [f**(e*2) for f, e in p_fact]
    coeffs = [1] * len(facts_exp)

    prod = facts_exp[0]
    for i in range(1, len(facts_exp)):
        coeff2, coeff1 = extended_gcd(prod, facts_exp[i])
        for j in range(i):
            coeffs[j] *= coeff1
        coeffs[i] = coeff2

        prod *= facts_exp[i]

    return list(zip(coeffs, facts_exp, [f for f, e in p_fact]))


def decomp_matrix(n):
    primes = prime_under(n+1)
    primes_exp = [p ** (int(log(n, p))*2) for p in primes]

    mat = [[0] * (n-1) for _ in range(len(primes))]

    for i in range(2, n+1):
        decomp = decompose(i)
        for coeff, fact_exp, fact in decomp:
            idx = primes.index(fact)
            mat[idx][i-2] = coeff * (primes_exp[idx] // fact_exp)

    return mat, primes_exp


def solve(n):
    mat, primes_exp = decomp_matrix(n)
    rows, cols = len(mat), len(mat[0])
    res = [-1] * cols

    def solve_rows(k, i, row_sum, partial_sum):
        if i == cols:
            if k == 0:          # 2
                return F(1, 2) == partial_sum + F(row_sum, primes_exp[0])
            if row_sum % primes_exp[k] == 0:
                return solve_rows(k-1, 0, 0,
                                  partial_sum + row_sum//primes_exp[k])
            return 0
        if mat[k][i] == 0:
            return solve_rows(k, i+1, row_sum, partial_sum)
        if res[i] != -1:
            return solve_rows(k, i+1, row_sum + mat[k][i]*res[i], partial_sum)

        count = 0
        res[i] = 0
        count += solve_rows(k, i+1, row_sum, partial_sum)
        res[i] = 1
        count += solve_rows(k, i+1, row_sum + mat[k][i], partial_sum)
        res[i] = -1
        return count

    return solve_rows(rows-1, 0, 0, 0)


def main():
    print(solve(45))
    sys.setrecursionlimit(2000)
    print(solve(80))


if __name__ == '__main__':
    main()
