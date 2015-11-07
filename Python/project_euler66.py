# Diophantine equation
"""
Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in 
positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, 
we obtain the following:

32 – 2×22 = 1
22 – 3×12 = 1
92 – 5×42 = 1
52 – 6×22 = 1
82 – 7×32 = 1

Hence, by considering minimal solutions in x for D ≤ 7, 
the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x 
for which the largest value of x is obtained.
"""

from math import sqrt
from fractions import Fraction
from project_euler64 import continued_fraction_period
from itertools import cycle, islice


def is_square(x):
    return x == int(sqrt(x)) ** 2


def pell(d):
    sequence = continued_fraction_period(d)
    # print(d, len(sequence), sequence)

    int_part = int(sqrt(d))
    x, y = int_part, 1
    i = 1
    while not check_pell(d, x, y):
        f = construct_fraction(d, list(islice(cycle(sequence), i)))
        x, y = f.numerator, f.denominator
        i += 1
    return x, y

    # for now, I cannot prove that if the sequence is divisible by 2, 
    # the following shortcut is correct
    # if len(sequence) % 2 == 0:
    #     f = construct_fraction(d, sequence[:-1])
    #     return f.numerator, f.denominator
    # else:
    #     int_part = int(sqrt(d))
    #     x, y = int_part, 1
    #     i = 1
    #     while not check_pell(d, x, y):
    #         f = construct_fraction(d, list(islice(cycle(sequence), i)))
    #         x, y = f.numerator, f.denominator
    #         i += 1
    #     return x, y


def construct_fraction(d, sequence):
    f = Fraction()
    for i in reversed(sequence):
        f = 1 / (f + i)
    f += int(sqrt(d))
    return f


def check_pell(d, x, y):
    return x ** 2 == d * y ** 2 + 1


def main():
    # max_x = 0
    # max_d = 0
    # for i in range(2, 1001):
    #     if is_square(i):
    #         continue
    #     this_x, this_y = pell(i)
    #     # print("D =", i, ", X =", this_x, ", Y =", this_y)
    #     if this_x > max_x:
    #         max_x = this_x
    #         max_d = i

    max_x, max_d = max((pell(i), i) for i in range(2, 1001) if not is_square(i))
    print(max_x, max_d, sep='\n')


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
