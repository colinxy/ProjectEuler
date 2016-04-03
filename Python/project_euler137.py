# project euler 137: Fibonacci golden nuggets

"""
Fib power series given by sigma(F[k] * x**k)
converges when x < 1/phi

S = A_F(x) = x / (1 - x - x**2)

For x to be rational in equation
S * x**2 + (S+1) * x - S = 0
delta = (S+1)**2 + 4 * S * S = (S+1)**2 + (2*S)**2
delta is perfect square

(s+1)**2 + (2*s)**2 = q**2
5 * s**2 + 2 * s + 1 = q**2
(5*s+1)**2 - 5 * q**2 = -4

solve pell equation of the form
u**2 - 5 * v**2 = -4

because odd fundamental solution (1, 1) exists,
equation can be reduced to easier form
x**2 - d * y**2 = -1

https://en.wikipedia.org/wiki/Pell%27s_equation#Transformations

if u**2 - d * v**2 = -4 and {x, y} = {(u**2+3)*u/2, (u**2+1)*v/2},
then x**2 - d * y**2 = -1
"""

import math
from fractions import Fraction
import pell


def is_square(x):
    return int(math.sqrt(x)) ** 2 == x


def rational_quad_solve(a, b, c):
    """
    a, b, c: int
    try to solve quadratic equation a * x**2 + b * x + c = 0
    if rational solution exist, return a tuple of 2 roots
    if rational solution does not exist, return None
    """
    delta = b * b - 4 * a * c
    if not is_square(delta):
        return None
    delta_sqrt = int(math.sqrt(delta))
    return (Fraction(-b - delta_sqrt, 2 * a),
            Fraction(-b + delta_sqrt, 2 * a))


def fib_series(x):
    return x / (1 - x - x * x)

N = 15


def main():
    # for s in range(2, 10000):
    #     sol = rational_quad_solve(s, s + 1, -s)
    #     if sol is not None:
    #         x = list(filter(lambda x: x > 0, sol))[0]
    #         # print(s, x)
    #         # print(s + 1, 2 * s, int(math.sqrt((s + 1)**2 + 4 * s * s)))
    #         u = s * 5 + 1
    #         print(u, (u ** 2 + 3) * u // 2)

    # Find Solution
    # u**2 - 5 * v**2 = -4
    # x = (u**2+3) * u / 2
    # y = (u**2+1) * v / 2

    # fundamental solution to pell equation
    fund_x, fund_y = pell.pell(5, True)
    x, y = fund_x, fund_y

    # solution exists for every 2
    for i in range(N * 2):
        x, y = pell.next_solution(5, fund_x, fund_y, x, y, True)

    # solve equation of the form 2*x = u**3 + 3 * u
    # intuition: 2*x  is close to  2*u**3

    # because of floating point inprecision,
    # starting from (2*x)**(1/3) is closer
    for u in range(int((2 * x)**(1 / 3)), int((2 * x)**(1 / 3)) * 2):
        # print((u**2 + 3) * u - x * 2)
        if (u**2 + 3) * u == x * 2:
            break

    # print(x, y, u)
    print((u - 1) // 5)


if __name__ == '__main__':
    main()
