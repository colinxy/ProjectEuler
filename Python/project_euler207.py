from math import sqrt, floor, log2
from fractions import Fraction
# import matplotlib.pylab as pl
# import numpy as np


def p_tuple(m):
    # n = 2 ** t
    # n * (n - 1) <= m
    n_max = floor(sqrt(m))
    if n_max * (n_max + 1) <= m:
        n_max += 1
    return floor(log2(n_max)), n_max - 1


def p_fr(m):
    return Fraction(p(m))


def p(m):
    n_max = floor(sqrt(m))
    if n_max * (n_max + 1) <= m:
        n_max += 1
    return floor(log2(n_max)) / (n_max - 1)


def main():
    threshhold = Fraction(1, 12345)
    # x = np.arange(5, 20000, 5)
    # @np.vectorize
    # def func(a):
    #     b = p(a)
    #     return b[0] / b[1]
    # y = func(x)
    # print(y[-1])
    # pl.plot(x, y)
    # pl.show()

    # research
    # print("{:.16f}".format(1 / 12345))
    # print("{:.16f}".format(p(10**10)))
    # print("{:.16f}".format(p(10**11)))
    # print("research show that answer is between 10**10 and 10**11")

    down = 10**10
    top = 10**11
    while True:
        mid = (top + down) // 2
        p_m = p_fr(mid)
        if p_m > threshhold:
            down = mid
        else:
            top = mid
        if top - mid == 1:
            if p_m >= threshhold >= p(top):
                print(top)
                break
            else:
                print("{:.16f} {}\n{:.16f} {}\n{:.16f} {}".format(p(top), top, p(mid), mid, p(down), down))
                raise Exception


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time() - starting_time, "seconds")
