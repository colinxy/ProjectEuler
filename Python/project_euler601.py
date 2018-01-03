# Problem 601: Divisibility streaks

from __future__ import division

from mathutil import gcd


def least_common_multipler(S):
    lcm = [1]
    for s in range(2, S+1):
        lcm.append(lcm[-1]*s // gcd(lcm[-1], s))
    return lcm


def P(s, N):
    mult, unmult = least_common_multipler(s+1)[-2:]
    res = (N-2)//mult - (N-2)//unmult
    return res


def sum_Ps(S, Ns):
    lcms = least_common_multipler(S+1)
    total = 0
    for mult, unmult, N in zip(lcms, lcms[1:], Ns):
        total += (N-2)//mult - (N-2)//unmult
    return total


def main():
    N = 31
    # print(sum(P(i, 4**i) for i in range(1, N+1)))

    print(sum_Ps(N, [4**i for i in range(1, N+1)]))


if __name__ == '__main__':
    main()
