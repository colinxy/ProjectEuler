# Problem 155: Counting Capacitor Circuits

from __future__ import print_function
from __future__ import division

from fractions import Fraction
from itertools import product

N = 18
cache_split = {1: [[1]]}        # Dict[int, List[List[int]]]
cache_parallel = {}             # Dict[int, Set[int]]
cache_series = {}               # Dict[int, Set[int]]


def cached(cache):

    def wrapped(func):
        def inner(n):
            if n not in cache:
                cache[n] = func(n)
            return cache[n]
        return inner

    return wrapped


def _split(n, start=1):
    if start > n:
        return []

    res = []
    res.append([n])
    for first in range(start, n//2+1):
        for latter in _split(n-first, first):
            latter.append(first)
            res.append(latter)
    return res


split = cached(cache_split)(_split)


@cached(cache_parallel)
def parallel(n):
    if n == 1:
        return {1}

    res = set()
    for splitted in split(n):
        if len(splitted) == 1:
            # special case, not splitting at all
            res.add(splitted[0])
            continue
        each_series = [series(i) for i in splitted]
        for each in product(*each_series):
            res.add(sum(each))

    return res


@cached(cache_series)
def series(n):
    if n == 1:
        return {1}

    res = set()
    for splitted in split(n):
        if len(splitted) == 1:
            # special case, not splitting at all
            res.add(splitted[0])
            continue
        each_parallel = [parallel(i) for i in splitted]
        for each in product(*each_parallel):
            res.add(
                Fraction(1, sum(Fraction(1, i) for i in each))
            )

    return res


def d_exact(n):
    # using exactly n capacitors
    return len(parallel(n) | series(n))


def d(n):
    # using up to n capacitors
    values = set()
    for i in range(1, n+1):
        exactly = parallel(i) | series(i)
        # print(exactly)
        values |= exactly
    return len(values)


def main():
    # print(d(1))
    # print(d(2))
    # print(d(3))

    # pypy 1.5min, slow
    print(d(N))


if __name__ == '__main__':
    main()
