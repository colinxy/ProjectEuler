# translated from my haskell solution, but faster (how?)
# NOTE: needs lru_cache (not available in python2)

from collections import defaultdict
from functools import lru_cache

from mathutil import nCr


remainders = [pow(10, x, 23) - 1 for x in range(22)]


@lru_cache(maxsize=None)
def partition_length(n):
    if n == 0:
        return {0: 1}
    res = defaultdict(int)
    for i in range(1, min(9, n)+1):
        for length, count in partition_length(n-i).items():
            res[length+1] += count
    return res


@lru_cache(maxsize=None)
def partition_maxlen(n, maxlen):
    total = 0
    for length, count in partition_length(n).items():
        if length > maxlen:
            continue
        total += count * nCr(maxlen, length)
    return total


def linear_combo(remainders, limits, maxlens):
    def lc_range(i, j):
        if i == j:
            return {}
        if i+1 == j:
            return {
                (remainders[i] * c % 23, c): partition_maxlen(c, maxlens[i])
                for c in range(min(23, limits[i])+1)
            }

        res = defaultdict(int)
        rem_left = lc_range(i, (i+j)//2)
        rem_right = lc_range((i+j)//2, j)
        for (rem1, use1), count1 in rem_left.items():
            for (rem2, use2), count2 in rem_right.items():
                if use1+use2 > 23:
                    continue
                res[((rem1+rem2) % 23, use1+use2)] += count1*count2

        return res

    return lc_range(0, len(limits))[0, 23]


def solve(n):
    maxlen, rem = divmod(n, 22)
    maxlens = ([maxlen+1] * rem) + ([maxlen] * (22-rem))
    limits = [ml*9 for ml in maxlens]
    return linear_combo(remainders, limits, maxlens)


def main():
    # print(solve(9))
    # print(solve(42))
    print(solve(11**12))


if __name__ == '__main__':
    main()
