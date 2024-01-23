
from __future__ import division

import random
from functools import reduce
import operator

from math import gcd
from math import prod as product
from math import comb as nCr
from math import perm as nPr

__author__ = 'yxy'


# deprecate product in favor of math.prod


def product_mod(iterable, mod, start=1):
    return reduce(lambda a, b: a * b % mod, iterable, start)


def fib_lazy():
    """Lazily generate fib sequence for fun and profit
    http://joelgrus.com/2015/07/07/haskell-style-fibonacci-in-python/
    Code is made python2/3 compatible
    """
    from operator import add
    from itertools import tee, islice
    try:
        from itertools import imap as map
    except ImportError:
        pass

    yield 1
    yield 1
    fibs1, fibs2 = tee(fib_lazy())
    for f in map(add, fibs1, islice(fibs2, 1, None)):
        yield f


# deprecate gcd in favor of math.gcd


def extended_gcd(n, p):
    """return (a, b) s.t. a*n + b*p == gcd(n, p)
    """
    a_, b_ = (1, 0), (0, 1)
    while p != 0:
        div = n // p
        a_, b_ = b_, (a_[0] - div*b_[0], a_[1] - div*b_[1])
        n, p = p, n - p*div
    return a_                    # tuple


# deprecate nCr in favor of math.comb


# deprecate nPr in favor of math.perm


def to_base(n, base, alphabet="0123456789abcdefghijklmnopqrstuvwxyz"):
    rep = ''
    while n > 0:
        n, digit = divmod(n, base)
        rep = alphabet[digit] + rep
    return rep


def is_prime(n):
    if n <= 3:
        return n >= 2
    if n % 2 == 0 or n % 3 == 0:
        return False
    for i in range(5, int(n ** 0.5) + 1, 6):
        if n % i == 0 or n % (i + 2) == 0:
            return False
    return True


def is_prob_prime(p, n=20):
    """Fermat primality test"""
    return all(pow(random.randint(1, p - 1), p - 1, p) == 1
               for _ in range(n))


def _try_composite(p, wit, d, r):
    """return true if definitely composite"""
    if pow(wit, d, p) == 1:
        return False
    return not any(pow(wit, 2**i * d, p) == p-1 for i in range(r))


def miller_rabin(p):
    """correct up to 341,550,071,728,321
    p is composite if any of the 2 following test fails
    witness ** (2**r * d) % p == 1, or
    witness ** (2**r * d) % p == p-1
    """
    if p <= 3:
        return p >= 2

    _SMALL_PRIMES = [
        2, 3, 5, 7, 11, 13, 17, 19, 23,
        29, 31, 37, 41, 43, 47, 53, 59,
        # 61, 67, 71, 73, 79, 83, 89, 97
    ]

    if p in _SMALL_PRIMES:
        return True
    if any((p % prime) == 0 for prime in _SMALL_PRIMES):
        return False

    d, r = p-1, 0
    while not d & 1:
        d >>= 1
        r += 1

    return not any(_try_composite(p, wit, d, r)
                   for wit in [2, 3, 5, 7, 11, 13, 17])


def prime_factorization(n):
    result = {}

    if n % 2 == 0:
        exp = 1
        n //= 2
        while n % 2 == 0:
            exp += 1
            n //= 2
        result[2] = exp

    factor = 3
    while factor * factor <= n:
        if n % factor == 0:
            exp = 1
            n //= factor
            while n % factor == 0:
                exp += 1
                n //= factor
            result[factor] = exp
        factor += 2

    if n != 1:
        result[n] = 1

    return result


def get_factors(n):
    """Not sorted"""
    p_facts = sorted(prime_factorization(n).items())
    facts = [1]

    for p_fact, exp in p_facts:
        facts = [f*p_fact**e for e in range(0, exp+1) for f in facts]

    return facts


def prime_under(ceiling):
    primes = [2]
    primality = [True] * ceiling
    primality[0], primality[1] = False, False
    for i in range(4, ceiling, 2):
        primality[i] = False
    for i in range(3, ceiling, 2):
        if primality[i]:
            primes.append(i)
            for j in range(i * i, ceiling, i * 2):
                primality[j] = False

    return primes


def is_prime_array(ceiling):
    import numpy as np

    is_prime_arr = np.ones(ceiling, dtype=np.bool)
    is_prime_arr[0], is_prime_arr[1] = False, False
    for i in range(4, ceiling, 2):
        is_prime_arr[i] = False
    for i in range(3, ceiling, 2):
        if is_prime_arr[i]:
            for j in range(i * i, ceiling, i * 2):
                is_prime_arr[j] = False
    return is_prime_arr


def prime_factors_under(ceiling):
    factor = [[] for _ in range(ceiling)]
    for i in range(2, ceiling):
        if not factor[-1]:
            factor[-1].append(i)
            yield factor.pop()
            for j in range(-i, -len(factor), -i):
                factor[j].append(i)
        else:
            yield factor.pop()


def prime_factorization_under(ceiling):
    factor = [{} for _ in range(ceiling)]
    for i in range(2, ceiling):
        if not factor[-1]:
            factor[-1][i] = 1
            yield factor.pop()
            for j in range(-i, -len(factor), -i):
                factor[j][i] = 1
        else:
            for fac in factor[-1]:
                number = i // fac
                while number % fac == 0:
                    factor[-1][fac] += 1
                    number //= fac
            yield factor.pop()
