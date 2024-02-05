# Similar to pe407
#
# Reference: https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
#
# a*a = 1 (mod n)  a < n-1
# (a-1)(a+1) = 0 (mod n)
# If a is odd, then gcd(a-1, a+1) = 2, and (a-1)(a+1) is a multiple of 8.
# If a is even, then gcd(a-1, a+1) = 1
#
# Let I'(n) be the set of all solutions to the modular equation.
#
# If n is odd and has only 1 prime factor, then I'(n) = {1, n-1}
#
# If n is odd and has more than 1 prime factors, then write n = pq, where p and q are co-prime.
# Solve system of equations using CRT:
# a = 1 (mod p), a = -1 = q-1 (mod q)
# Let p1 = p^-1 (mod q), q1 = q^-1 (mod p)
# a = (q-1) p p1 + q q1 (mod pq)
#
# If n is a power of 2, then I' has at most 4 solutions.
#
# If n is even, then we can remove powers of 2 from n to get n'. Solve
# n' as odd cases above to get a'.
#
# Then consider powers of 2 of n:
# Let g be the largest powers of 2 divisible by n, and h = g/2.
# One of (a-1) and (a+1) is divisible only by 2, but not 4;
# the other is divisible by at least h.
# Now we get (a-1)(a+1) divisible by g.
#
# Then we need to solve system of equations:
# a = a' (mod n'), a = 1 or -1 (mod h)
# However, this doesn't give us all solutions mod n, because n'h = n/2.
# a and a + n/2 are both solutions.

from math import prod
from itertools import combinations
from mathutil import extended_gcd


def I(n):
    for i in range(n-2, 0, -1):
        if i*i % n == 1:
            return i


def prime_powers_skip2_under(n):
    prime_powers = [[] for _ in range(n)]

    # skip all even numbers
    for p in range(3, n, 2):
        if len(prime_powers[p]) != 0:
            continue

        # p is a prime
        for j in range(p, n, p*2):
            num = j
            powers = 1
            while num % p == 0:
                powers *= p
                num //= p
            prime_powers[j].append(powers)

    return prime_powers


def crt(a, b, p, q):
    """
    Chinese Remainder Theorem

    Solve x mod pq:
    x = a (mod p)
    x = b (mod q)
    p and q co-prime
    """
    p_inv, q_inv = extended_gcd(p, q)
    return (a*q*q_inv + b*p*p_inv) % (p*q)


def self_invs(n, prime_powers):
    if n % 2 == 0:
        if len(prime_powers) == 1:
            return self_invs_pow2(n)
        pow2 = prime_powers[0]
        self_invs_odd = self_invs(n//pow2, prime_powers[1:])
        return self_invs_even(n, pow2, self_invs_odd)

    # n is odd
    results = [1, n-1]
    if len(prime_powers) == 1:
        return results

    for i in range(1, len(prime_powers)//2+1):
        for c in combinations(prime_powers, i):
            p = prod(c)
            q = n // p
            p_inv, q_inv = extended_gcd(p, q)
            p_inv %= q
            q_inv %= p

            results.append(((q-1)*p*p_inv + q*q_inv) % n)
            if i*2 != len(prime_powers):
                results.append(((p-1)*q*q_inv + p*p_inv) % n)

    return sorted(results)


def self_invs_pow2(n):
    # n is a power of 2, n > 2
    if n == 4:
        return [1, 3]
    return [1, n//2-1, n//2+1, n-1]


def self_invs_even(n, pow2, self_invs_odd):
    results = []
    for self_inv in self_invs_odd:
        results.append(crt(self_inv, 1, n//pow2, pow2//2))
        if pow2 > 4:
            results.append(crt(self_inv, -1, n//pow2, pow2//2))

    if pow2 == 2:
        return sorted([
            res + o
            for o in [0, n//2]
            for res in results
            if (res + o) % 2 == 1
        ])

    return sorted([
        res + o
        for o in [0, n//2]
        for res in results
    ])


def self_inv_sieve(n):
    prime_powers = prime_powers_skip2_under(n+1)

    total = 0
    # powers of 2 after 4
    i = 4
    while i <= n:
        total += self_invs_pow2(i)[-2]
        i *= 2
    # for every odd i, and their multiples of 2's powers
    for i in range(3, n+1, 2):
        self_invs_i = self_invs(i, prime_powers[i])
        total += self_invs_i[-2]

        j = i * 2
        pow2 = 2
        while j <= n:
            total += self_invs_even(j, pow2, self_invs_i)[-2]
            j *= 2
            pow2 *= 2

    return total


if __name__ == '__main__':
    print(self_inv_sieve(2*10**7))
