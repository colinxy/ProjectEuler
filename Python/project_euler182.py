# Reference:
# https://crypto.stanford.edu/pbc/notes/numbertheory/crt.html
# https://en.wikipedia.org/wiki/RSA_(cryptosystem)#cite_note-22
# https://en.wikipedia.org/wiki/Carmichael_function
#
# Intuition 1: by Chinese Remainder Theorem, there can only be a
# unique solution x (mod pq) to the remainder equations
#
# m = m^e (mod pq)
# iff
# m = m^e (mod p) AND m = m^e (mod q)
#
# This means:
# we only need to study the structure of Z_p and Z_q instead of Z_pq
#
#
# Intuition 2: there are at least 9 unconcealed messages
# These 9 m satisfies
# m = r1 (mod p)  AND
# m = r2 (mod q)
# where r1 = 0 or 1 or -1, r2 = 0 or 1 or -1
#
# (Proof)
#
# Since e is odd (-1^e = -1)
# m^e = r1^e = r1 (mod p)  AND
# m^e = r2^e = r2 (mod q)
#
# From Intuition 1, m = m^e (mod pq)
#
# Intuition 3: for certain e, suppose there are additional unconcealed
# messages than the 9 above.
#
# Suppose m and p are co-prime, we want to find m such that m^(e-1) =
# 1 (mod p)
# we don't have to worry about q because we can always find m' such that
# m' = m (mod p)
# m' = 1 (mod q)
# m' is unconcealed in Z_pq
#
# let m0 be a unit whose order is p-1
# Note: order is the smallest x that m0^x = 1 (mod p)
# (Carmichael's theorem shows there must exist such m0 for prime p)
# m0 is not 0 or 1
#
# let k = gcd(e-1, p-1), 2 is a factor of k since e and p are odd.
# m0^((p-1)/k * (e-1)) = m0^((p-1) * (e-1)/k) = 1 (mod p)
# therefore we find m = m0^((p-1)/k) (mod p)
#
# In fact, if k = 2, m = -1 (mod p)
# If k > 2, we can find additional unconcealed messages than the 9 above.

from math import gcd
from mathutil import prime_factorization


def totient(n, factors=None):
    if not factors:
        factors = prime_factorization(n)
    for f in factors:
        n = n // f * (f-1)
    return n


def unconcealed_msg(n, e):
    msgs = [0, 1]

    for m in range(2, n):
        if m == pow(m, e, n):
            msgs.append(m)

    return msgs


def is_min_unconcealed(p, q, e):
    return gcd(e-1, p-1) == 2 and gcd(e-1, q-1) == 2


def all_min_e(p, q):
    tot = totient(p*q, factors=[p, q])

    es = []
    for e in range(3, tot, 2):
        if gcd(e, tot) != 1:
            continue
        if is_min_unconcealed(p, q, e):
            es.append(e)

    return es


if __name__ == '__main__':
    print(sum(all_min_e(1009, 3643)))
