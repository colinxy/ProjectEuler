# Problem 160: Factorial trailing digits

from __future__ import print_function
from __future__ import division

from math import log

try:
    range = xrange
except NameError:
    pass

MOD = 10 ** 5


def f_slow(n):
    twos = sum(n // (2**i) for i in range(int(log(n, 2))+1))
    fives = sum(n // (5**i) for i in range(int(log(n, 5))+1))

    res = 1
    for i in range(1, n+1):
        while i % 2 == 0:
            i //= 2
        while i % 5 == 0:
            i //= 5
        res = res * i % MOD
    return res * pow(2, twos-fives, MOD) % MOD


below100000 = 1
for i in range(1, MOD+1):
    if i % 2 == 0 or i % 5 == 0:
        continue
    below100000 = below100000 * i % MOD


def notmult2_5(n):
    div, mod = divmod(n, MOD)

    res = 1
    for i in range(1, mod+1):
        if i % 2 == 0 or i % 5 == 0:
            continue
        res = res * i % MOD

    return res * pow(below100000, div, MOD) % MOD


def f(n):
    non2_5 = 1

    for pow2s in range(int(log(n, 2))+1):
        for pow5s in range(int(log(n, 5))+1):
            pow2s5s = 2**pow2s * 5**pow5s
            if pow2s5s > n:
                break

            non2_5 = non2_5 * notmult2_5(n//pow2s5s) % MOD

    twos = sum(n // (2**i) for i in range(int(log(n, 2))+1))
    fives = sum(n // (5**i) for i in range(int(log(n, 5))+1))

    return non2_5 * pow(2, twos-fives, MOD) % MOD


def main():
    # print(f(10**5))
    # print(f_slow(10**5))
    print(f(10**12))


if __name__ == '__main__':
    main()
