"""
This module solve pell equation of the form

 2        2
x  - d * y  = 1

and

 2        2
x  - d * y  = -1

Example:

    >>> import pell
    >>> x, y = pell.pell(10)
    >>> x, y
    (19, 6)
    >>> x, y = pell.pell(61)
    >>> x, y
    (1766319049, 226153980)
"""

from math import sqrt
from mathutil import gcd
from fractions import Fraction
from itertools import cycle, islice


def is_square(n):
    return n == int(sqrt(n)) ** 2


def check_pell(d, x, y):
    return x ** 2 == d * y ** 2 + 1


def _continued_fraction_period(n):
    """
    find the continued fraction period for sqrt(n)
    
    Example:
    
    >>> _continued_fraction_period(23)
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

    while True:
        int_part, x, y, z = _next_continued_fraction(x, y, z)
        if (x, y, z) == (first_x, first_y, first_z):
            return sequence
        sequence.append(int_part)


def _next_continued_fraction(x, y, z=1, w=1):
    """
    w should be 1

    (√x + y) * w
    ------------
          z
    """
    int_part = int(z * (sqrt(x) - y) / (w * (x - y ** 2)))
    # get reciprocal of the fraction
    # numerator: z * (sqrt(x) - y)
    # denominator: w * (x - y**2)
    common_divisor = gcd(z, w * (x - y ** 2))
    w = w * (x - y ** 2) // common_divisor
    z //= common_divisor
    y = -y - int_part * w
    assert z != 1, 'x={}, y={}, z={}, w={}'.format(x, y, z, w)
    return int_part, x, y, w


def _construct_fraction(d, sequence):
    f = Fraction()
    for i in reversed(sequence):
        f = 1 / (f + i)
    f += int(sqrt(d))
    return f


def pell(d):
    sequence = _continued_fraction_period(d)

    int_part = int(sqrt(d))
    x, y = int_part, 1
    i = 1
    while not check_pell(d, x, y):
        f = _construct_fraction(d, list(islice(cycle(sequence), i)))
        x, y = f.numerator, f.denominator
        i += 1
    return x, y
