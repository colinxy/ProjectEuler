#
# 0 <= a < n
# a*a - a = 0 (mod n)
# a(a-1) = 0 (mod n)
#
# If n is prime, then n | a or n | a-1
# Since a < n, this corresponds to the only solutions a = 0 or a = 1
# Similarly for n that are prime powers.
#
# Consider M' to be all solutions to the modular equation,
# then M = max(M')
#
# As shown above, for primes or prime powers n, M'(n) = {0, 1}
#
# Suppose n has multiple prime factors. We can find n = pq, where p, q
# co-prime, such that p | a and q | a-1 (a and a-1 are co-prime)
#

from math import prod
from itertools import combinations
from mathutil import extended_gcd


def M(n):
    for a in range(n-1, -1, -1):
        if a*a % n == a:
            return a


def mod_inverse(n, mod):
    inv, _ = extended_gcd(n, mod)
    # inv might be negative, so take another modular to ensure it's positive
    return inv % mod


def M_powers(n, prime_powers):
    # n = p1^r1 * p2^r2 * ... * pk^rk
    # prime_powers = [p1^r1, p2^r2, ..., pk^rk]
    if len(prime_powers) == 1:
        return 1

    m_max = 1
    for i in range(1, len(prime_powers)//2+1):
        for c in combinations(prime_powers, i):
            p1 = prod(c)
            p2 = n // p1
            pqs = [(p1, p2)]
            if i*2 != len(prime_powers):
                pqs.append((p2, p1))

            for p, q in pqs:
                m = p * mod_inverse(p, q)
                m_max = max(m_max, m)

    return m_max


def prime_powers_under(n):
    prime_powers = [[] for _ in range(n)]

    for p in range(2, n):
        if len(prime_powers[p]) != 0:
            continue

        # p is a prime
        for j in range(p, n, p):
            num = j
            powers = 1
            while num % p == 0:
                powers *= p
                num //= p
            prime_powers[j].append(powers)

    return prime_powers


def M_sieve(n):
    prime_powers = prime_powers_under(n+1)
    return [
        M_powers(i, prime_powers[i])
        for i in range(2, n+1)
    ]


if __name__ == '__main__':
    print(sum(M_sieve(10**7)))
