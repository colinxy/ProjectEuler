# project euler 140: Modified Fibonacci golden nuggets

"""
Modifies Fib series

S = A_G(x) = (3 - 2*x) / (1 - x - x**2) - 3

for x to be rational in equation
(S+3) * x**2 + (S+1) * x - S = 0
delta = (S+1)**2 + 4 * (S+3) * S
      = 5 * S**2 + 14 * S + 1 = t ** 2

delta is perfect square

(5*s+7)**2 - 5 * t ** 2 = 44
reduced to
u**2 - 5 * v**2 = 44
has trivial solution (7, 1)

Reference:
http://mathworld.wolfram.com/PellEquation.html  (42)

one fundamental solution does not necessarily generate all solutions

given x**2 - 5 * y**2 = 1
and   (u + √5v) (u - √5v) = 44
find next solution
((u + √5v)(x + √5y)) ((u - √5v)(x - √5y)) = 44
((ux+5vy) + √5(vx+uy)) ((ux+5vy) + √5(vx+uy)) = 44
"""

import math
import pell


def is_square(x):
    return int(math.sqrt(x)) ** 2 == x

N = 30


def main():
    # for i in range(2, 10000):
    #     if is_square(5 * i**2 + 14 * i + 1):
    #         print(i)
    #         print(i * 5 + 7, math.sqrt(((i * 5 + 7)**2 - 44) // 5))

    # pell equation u**2 - 5 * v**2 = 44
    # fundamental solutions found through trial
    fund_uv_s = [(17, 7), (32, 14), (112, 50),
                 (217, 97), (767, 343), (1487, 665)]
    fund_x, fund_y = pell.pell(5)

    nuggets = []
    for u, v in fund_uv_s:
        for _ in range(N):
            if u % 5 == 2:
                nuggets.append((u - 7) // 5)
                # print((u - 7) // 5, total)

            u, v = u * fund_x + 5 * v * fund_y, v * fund_x + u * fund_y
            # print(u, v)
            # print((u - 7) / 5)

    nuggets.sort()
    # print(nuggets)
    print(sum(nuggets[:N]))


if __name__ == '__main__':
    main()
