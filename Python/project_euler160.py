# Problem 160: Factorial trailing digits

from __future__ import print_function
from __future__ import division

from math import log


def prod_mod_strip0(top):
    # pow2 = int(log(top, 2))
    # twos = sum(top // (2 ** i) for i in range(1, pow2+1))
    pow5 = int(log(top, 5))
    fives = sum(top // (5 ** i) for i in range(1, pow5+1))

    # print(pow2)
    # print(twos)
    # print(pow5)
    # print(fives)

    prod_mod = 1
    reduce_twos = fives
    for i in range(1, top+1):
        while reduce_twos > 0 and i % 2 == 0:
            i //= 2
            reduce_twos -= 1
        while i % 5 == 0:
            i //= 5

        i %= 100000
        prod_mod = prod_mod * i % 100000

    # prod_mod = prod_mod * pow(2, two_left, 100000) % 100000

    return prod_mod


def f_slow(start, stop=None, mod=100000):
    if stop is None:
        stop = start
        start = 1
    prod_mod = 1
    for i in range(start, stop+1):
        # while i % 10 == 0:
        #     i //= 10
        # i %= mod
        prod_mod *= i
        while prod_mod % 10 == 0:
            prod_mod //= 10
        prod_mod %= mod

    return prod_mod


def main():
    N = 10 ** 12
    # need higher precision than 10**5
    MOD = 10 ** 12

    # only last 5 nonzero digits needs to be considered
    # 0000_ 1-9
    # 000__ 10-99
    # 00___ 100-999
    # 0____ 1000-9999
    # _____ 10000-99999

    last1 = f_slow(1, 9, mod=MOD)
    last2 = f_slow(10, 99, mod=MOD)
    last3 = f_slow(100, 999, mod=MOD)
    last4 = f_slow(1000, 9999, mod=MOD)
    last5 = f_slow(10000, 99999, mod=MOD)

    # print(last1 * last2 * last3 * last4 * last5 % 100000)

    # e.g. 6 digits
    # _|_____: first digit 1-9
    # _____|_: last digit 0
    # e.g. 7 digits
    # __|_____: first digit 1-9, second digit 0-9
    # _|_____|_: first digit 1-9, last digit 0
    # _____|__: last 2 digits 0

    print(f_slow(10**5, mod=MOD))
    print(pow(f_slow(10**5, mod=MOD), 10**7, 10**5))

    # from functools import reduce
    # reduce(lambda acc, x: acc*x % MOD, range(1, 10**8+1))


if __name__ == '__main__':
    main()
