# project euler 141: Investigating progressive numbers,
#                    n, which are also square


"""
perfect_sq = d * q + r
d * d == q * r

perfect_sq = r + d ** 3 / r  (r < d)

            3
 2         d
n  = r + -----
           r

parametrize

gcd(a, b) = 1, b > a
         2
r = c * a
d = c * a * b

 2        2    2        3
n  = c * a  + c  * a * b
"""

from __future__ import division
from math import sqrt
from mathutil import gcd

N = 10 ** 10     # not feasible for real problem size, see cpp version
SQRT_N = int(sqrt(N))
CBRT_N = int(N ** (1 / 3))


def is_square(n):
    return int(sqrt(n)) ** 2 == n


def parametrized():
    # actually a bit slower than unparametrized version
    prog_sq = []
    for b in range(2, CBRT_N):
        for a in range(1, b):
            if a * a * a * b * b > N:
                break
            if gcd(a, b) != 1:
                continue
            for c in range(1, SQRT_N):
                candidate = c * a * a + c * c * a * b * b * b
                if candidate > N:
                    break
                if is_square(candidate):
                    prog_sq.append(candidate)
                    print(candidate, c * a * b)

    print(sum(prog_sq))


def main():
    parametrized()

    # progressive_sum = 0
    # progressive_sq = []
    # for d in range(2, SQRT_N):
    #     for r in range(d, 0, -1):
    #         d_cubed = d ** 3
    #         if d_cubed % r != 0 or d * d % r != 0:
    #             continue

    #         candidate = d_cubed // r + r
    #         if candidate > N:
    #             break
    #         if is_square(candidate):
    #             progressive_sum += candidate
    #             progressive_sq.append(candidate)
    #             print(candidate, d, r)

    # print(progressive_sum)
    # print(sorted(progressive_sq))


if __name__ == '__main__':
    main()
