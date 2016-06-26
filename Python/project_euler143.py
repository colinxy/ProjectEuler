# project euler 143: Investigating the Torricelli point of a triangle

from __future__ import print_function, division
from math import sqrt
from equation import solve_quad

N = 12000
# too slow actual problem size, see cpp solution


def is_square(n):
    return int(sqrt(n))**2 == n


def main():
    # find all p, q pairs that produce
    # p**2 + q**2 + p**q = a**2
    # (p+q)**2 - p*q = a**2

    p_q_pairs = []
    # for p in range(1, N//2):
    #     p_sq = p * p
    #     for q in range(p+1, N - p):
    #         p_q_sq = p_sq + q * (p + q)
    #         if is_square(p_q_sq):
    #             p_q_pairs.append((p, q))

    p_q_pairs = []
    for p_plus_q in range(1, N):
        p_plus_q_sq = p_plus_q * p_plus_q
        for a in range(p_plus_q-1, 1, -1):
            # (p+q)**2 / 4 >= p*q
            p_mult_q = p_plus_q_sq - a*a
            if p_plus_q_sq / 4 < p_mult_q:
                break
            p, q = solve_quad(1, -p_plus_q, p_mult_q)
            p = int(p+0.5)
            q = int(q+0.5)
            if p+q == p_plus_q and p*q == p_mult_q:
                p_q_pairs.append((p, q))

    # print(p_q_pairs)
    # print(p_q_pairs)
    # print(len(p_q_pairs))


if __name__ == '__main__':
    main()
