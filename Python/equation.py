"""
this module provides utilities to solve polynomial equations
"""

from math import sqrt


def solve_quad(a, b, c, imag=False):
    """solve quadratic equation of the form
    a * x**2 + b * x + c = 0
    """
    delta = b * b - 4 * a * c

    if imag and delta < 0:
        delta = -delta

    # for negative delta, an exception will be thrown
    delta_sqrt = sqrt(delta)
    x1 = (-b - delta_sqrt * (1j if delta < 0 else 1)) / (2 * a)
    x2 = (-b + delta_sqrt * (1j if delta < 0 else 1)) / (2 * a)
    return x1, x2


def solve_cubic_depressed(p, q):
    """solve depressed cubic equation using Vieta's substitution
    x**3 + p*x + q = 0

    Algorithm illustrated:
    x = t - p/(3*t)
    t**3 + q - (p**3)/(27*t**3) = 0
    t**6  + q * t**3 - p**3/27 = 0
    """
    t_cubed1, t_cubed2 = solve_quad(1, q, p**3 / 27)
    t1, t2 = t_cubed1**(1 / 3), t_cubed2**(1 / 3)
    # TODO : method still erroneous
    return t1 - p / (3 * t1), t2 - p / (3 * t2)
