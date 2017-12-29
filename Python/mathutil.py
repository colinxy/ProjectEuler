
from __future__ import division

import random
from functools import reduce
from itertools import combinations, tee, islice
import operator
from collections import defaultdict
import heapq

__author__ = 'yxy'

# __all__ = ['gcd', 'nCr', 'nPr', 'product',
#            'product_mod', 'to_base',
#            'is_prime', 'is_prob_prime',
#            'prime_under', 'prime_factorization',
#            'factors', 'prime_factors_under',
#            'prime_factors_under_lazy_heap',
#            'prime_factors_under_lazy_dict']


def product(iterable, start=1):
    return reduce(operator.mul, iterable, start)


def product_mod(iterable, mod, start=1):
    return reduce(lambda a, b: a * b % mod, iterable, start)


def fib_lazy():
    """Lazily generate fib sequence for fun and profit
    http://joelgrus.com/2015/07/07/haskell-style-fibonacci-in-python/
    Code is made python2/3 compatible
    """
    from operator import add
    try:
        from itertools import imap as map
    except ImportError:
        pass

    yield 1
    yield 1
    fibs1, fibs2 = tee(fib_lazy())
    for f in map(add, fibs1, islice(fibs2, 1, None)):
        yield f


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def extended_gcd(n, p):
    """return (a, b) s.t. a*n + b*p == gcd(n, p)
    """
    a_, b_ = (1, 0), (0, 1)
    while p != 0:
        div = n // p
        a_, b_ = b_, (a_[0] - div*b_[0], a_[1] - div*b_[1])
        n, p = p, n - p*div
    return a_                    # tuple


def nCr(m, n):
    """Choose n out of m"""
    if n > m / 2:
        n = m - n
    result = 1
    for i in range(1, n + 1):
        result *= m - i + 1
        result //= i
    return result


def nPr(m, n):
    """Arrange n out of m"""
    result = 1
    for i in range(m, m - n, -1):
        result *= i
    return result


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

_SMALL_PRIMES = [
    2, 3, 5, 7, 11, 13, 17, 19, 23,
    29, 31, 37, 41, 43, 47, 53, 59,
    # 61, 67, 71, 73, 79, 83, 89, 97
]


def miller_rabin(p):
    """correct up to 341 550 071 728 321
    p is composite if any of the 2 following test fails
    witness ** (2**r * d) % p == 1, or
    witness ** (2**r * d) % p == p-1
    """
    if p <= 3:
        return p >= 2

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


def factors(n):
    p_fact = sorted(prime_factorization(n).items())
    p_facts = []
    for fact, exp in p_fact:
        p_facts.extend([fact] * exp)

    for r in range(1, len(p_facts)):
        for j in combinations(p_facts, r):
            yield product(j)


def prime_under(ceiling):
    primes = []
    primality = [True] * ceiling
    primality[0], primality[1] = False, False
    for (i, isPrime) in enumerate(primality):
        if isPrime:
            primes.append(i)
            for j in range(i * i, ceiling, i):
                primality[j] = False

    return primes


def is_prime_array(ceiling):
    import numpy as np

    is_prime_arr = np.ones(ceiling, dtype=np.bool)
    is_prime_arr[0], is_prime_arr[1] = False, False
    for (i, isPrime) in enumerate(is_prime_arr):
        if isPrime:
            for j in range(i * i, ceiling, i):
                is_prime_arr[j] = False
    return is_prime_arr


# TODO : delete this, basically useless
def prime_factors_under_lazy_heap(ceiling):
    update = []  # a list of tuples, (num, p), num->next number that factor p

    heapq.heappush(update, (4, 2))
    yield [2]
    for i in range(3, ceiling):
        # i <= update[0][0]
        if update[0][0] > i:  # then i is prime
            heapq.heappush(update, (2 * i, i))
            yield [i]
        else:  # i is not prime, i == update[0][0]
            factor = []
            while update[0][0] == i:
                next_fac, p = update[0]
                factor.append(p)
                heapq.heapreplace(update, (next_fac + p, p))
            yield factor


# TODO : delete this, basically useless
def prime_factors_under_lazy_dict(ceiling):
    update = defaultdict(list)

    update[4] = [2]
    yield [2]
    for i in range(3, ceiling):
        if i not in update:  # then i is prime
            update[2 * i].append(i)
            yield [i]
        else:  # i is not prime
            factor = update.pop(i)
            yield factor
            for p in factor:
                update[i + p].append(p)


def prime_factors_under(ceiling):
    primality = [True] * ceiling
    factor = [[] for _ in range(ceiling)]
    for i in range(2, ceiling):
        if primality[i]:
            factor[-1].append(i)
            yield factor.pop()
            for j in range(i * i, ceiling, i):
                primality[j] = False
            for j in range(-i, -len(factor), -i):
                factor[j].append(i)
        else:
            yield factor.pop()


def prime_factorization_under(ceiling):
    primality = [True] * ceiling
    factor = [{} for _ in range(ceiling)]
    for i in range(2, ceiling):
        if primality[i]:
            factor[-1][i] = 1
            yield factor.pop()
            for j in range(i * i, ceiling, i):
                primality[j] = False
            for j in range(-i, -len(factor), -i):
                factor[j][i] = 1
        else:
            for fac in factor[-1]:
                number = i // fac
                while number % fac == 0:
                    factor[-1][fac] += 1
                    number //= fac
            yield factor.pop()
