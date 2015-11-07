
__author__ = 'yxy'

__all__ = ['gcd', 'nCr', 'nPr', 'product', 
           'product_mod', 'to_base',
           'is_prime', 'is_prob_prime', 
           'prime_under', 'prime_factorization',
           'factors', 'prime_factors_under',
           'prime_factors_under_lazy_heap',
           'prime_factors_under_lazy_dict']

import random
from fractions import gcd
from functools import reduce
from itertools import combinations
from operator import mul
from collections import defaultdict
import heapq
import numpy as np


def product(iterable, start=1):
    return reduce(mul, iterable, start)


def product_mod(iterable, mod, start=1):
    return reduce(lambda a, b: a * b % mod, iterable, start)


def nCr(m, n):
    result = 1
    for i in range(1, n+1):
        result *= m-n+1
        result //= i
    return result


def nPr(m, n):
    result = 1
    for i in range(m, m-n, -1):
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
    return all([pow(random.randint(1, p-1), p-1, p) == 1 for _ in range(n)])


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
    is_prime_arr = np.ones(ceiling, dtype=np.bool)
    is_prime_arr[0], is_prime_arr[1] = False, False
    for (i, isPrime) in enumerate(is_prime_arr):
        if isPrime:
            for j in range(i * i, ceiling, i):
                is_prime_arr[j] = False
    return is_prime_arr


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


# def factorization(n, start=2):
#     if start * start > n:
#         return [[n]]

#     the_factorization = [[n]]
#     for i in range(start, int(n ** 0.5) + 1):
#         if n % i == 0:
#             fac = factorization(n // i, i)
#             for each in fac:
#                 each.append(i)
#             the_factorization.extend(fac)
# 
#     return the_factorization


# def permutation(characters):
#     """
#     *DEPRECATED*, use itertools.permutations instead
#     
#     input: a list of characters, repeat allowed, counted only once
#     output: all possible permutations of the given list
#     """
#     if len(characters) == 1:
#         return [characters]

#     result = []
#     for i in range(len(characters)):
#         result.extend([j + [characters[i]] for j in permutation(characters[:i] + characters[i+1:])])

#     return result


# def permutation_formatted(numbers):
#     result = permutation(numbers)
#     return sorted(set([tuple(i) for i in result]))


# def gcd(x, y):
#     return x if y == 0 else gcd(y, x % y)
