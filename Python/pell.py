"""
This module find the fundamental solution 
to pell equation of the form

 2        2
x  - d * y  = 1

and

 2        2
x  - d * y  = -1

Example:

    >>> import pell
    >>> x, y = pell.pell(10)         # solve x**2 - d * y**2 = 1
    >>> x, y
    (19, 6)
    >>> x, y = pell.pell(61)
    >>> x, y
    (1766319049, 226153980)
    >>> x, y = pell.pell(101, True)  # solve x**2 - d * y**2 = -1
    >>> x, y
    (10, 1)
"""

from math import sqrt
from mathutil import gcd
from fractions import Fraction
from itertools import cycle, islice


def is_square(n):
    return n == int(sqrt(n)) ** 2


def check_pell(d, x, y, negative=False):
    return x ** 2 - d * y ** 2 == (-1 if negative else 1)


def continued_fraction_period(n):
    """
    find the continued fraction period for sqrt(n)
    
    Example:
    
    >>> continued_fraction_period(23)
    [1, 3, 1, 8]
    
    sqrt(23) = 
                1
    4 + -----------------
                  1
        1 + -------------
                    1
            3 + ---------
                      1
                1 + -----
                    8 + ...

    Algorithm illustrated:
    
    √23 = 4 + (√23 - 4)

    a0 = 4,
      1     √23+4       √23—3
    ----- = ----- = 1 + -----
    √23—4 	  7           7
    
    a1 = 1,
      7     √23+3       √23—3
    ----- = ----- = 3 + -----
    √23—3 	  2           2
    
    a2 = 3,
      2     √23+3       √23—4
    ----- = ----- = 1 + -----
    √23—3 	  7           7
    
    a3 = 1,
      7     √23+4       √23—4
    ----- = ----- = 8 + -----
    √23—4 	  1           1
    
    ...

               √x + y
    int_part + ------
                  z
    
    """
    
    # precondition n is not a perfect square
    if is_square(n):
        raise ValueError("Invalid Input: perfect square")

    sequence = []
    # (sqrt(x) + y) / z
    # intermediate term

    int_part, first_x, first_y, first_z = _next_continued_fraction(n, -int(sqrt(n)))
    sequence.append(int_part)
    x, y, z = first_x, first_y, first_z

    while True:
        int_part, x, y, z = _next_continued_fraction(x, y, z)
        if (x, y, z) == (first_x, first_y, first_z):
            return sequence
        sequence.append(int_part)


def _next_continued_fraction(x, y, z=1):
    """
    (√x + y)    reciprocal        z * (√x - y)        z * (√x - y)
    --------  =============>  --------------------- = ------------
        z                      (√x + y) * (√x - y)      x - y**2
    """
    assert x > y * y

    int_part = int(z * (sqrt(x) - y) / (x - y ** 2))
    common_divisor = gcd(z, x - y ** 2)

    denominator = (x - y ** 2) // common_divisor
    y = -y - int_part * denominator

    assert z == common_divisor, \
           'x={}, y={}, z={}'.format(x, y, z)

    return int_part, x, y, denominator


def _construct_fraction(d, sequence):
    f = Fraction()
    for i in reversed(sequence):
        f = 1 / (f + i)
    f += int(sqrt(d))
    return f


def pell(d, negative=False):
    """
    when neg == False, find fundamental solution to the equation
    x**2 - d * y**2 = 1
    
    when neg == True, find fundamental solution to the equation
    x**2 - d * y**2 = -1
    """
    sequence = continued_fraction_period(d)
    if negative and len(sequence) % 2 == 0:
        raise ValueError("No Solution: period of the continued fraction is even")

    int_part = int(sqrt(d))
    x, y = int_part, 1
    i = 1
    while not check_pell(d, x, y, negative):
        f = _construct_fraction(d, list(islice(cycle(sequence), i)))
        x, y = f.numerator, f.denominator
        i += 1
    return x, y


def next_solution(d, x_1, y_1, x_n, y_n):
    """
    find next solution to the pell equation
    x**2 - d * y**2 = 1
    
    Args:
        x_1, y_1: the fundamental Solution to the equation
        x_n, y_n: the previous Solution to the equation
    """
    return x_1*x_n + d*y_1*y_n, x_1*y_n + y_1*x_n
