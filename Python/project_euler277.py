# project euler 277: A Modified Collatz sequence

from fractions import Fraction
from mathutil import extended_gcd

N = 10 ** 15
# SEQ = "DdDddUUdDD"
SEQ = "UDDDUdddDDUDDddDdDddDDUDDdUUDd"


# brute force, infeasible for problem size
collatz_cache = {1: ''}


# brute force, infeasible for problem size
def collatz(n):
    if n in collatz_cache:
        return collatz_cache[n]

    if n % 3 == 0:
        collatz_seq = 'D' + collatz(n//3)
    elif n % 3 == 1:
        collatz_seq = 'U' + collatz((4*n+2) // 3)
    else:
        collatz_seq = 'd' + collatz((2*n-1) // 3)

    collatz_cache[n] = collatz_seq
    return collatz_seq


# for checking answer
def collatz_prefix(n, length):
    collatz_seq = ''
    for i in range(length):
        if n % 3 == 0:
            collatz_seq += 'D'
            n = n//3
        elif n % 3 == 1:
            collatz_seq += 'U'
            n = (4*n+2) // 3
        else:
            collatz_seq += 'd'
            n = (2*n-1) // 3
    return collatz_seq


def parametrize(sequence):
    # parametrize with (coeff1 * x + coeff2)
    coeff1, coeff2 = Fraction(1), Fraction(0)
    for s in sequence:
        if s == 'D':
            # x/3
            coeff1 *= 3
        elif s == 'U':
            # (4*x+2)/3
            coeff1, coeff2 = 3*coeff1/4, coeff2-coeff1/2
        else:                   # s == 'd'
            # (2*x-1)/3
            coeff1, coeff2 = 3*coeff1/2, coeff2+coeff1/2
    return coeff1, coeff2


def mod_solve(a, b, mod):
    """return solution for (a*x + b) % mod == 0
    * a and mod are coprime
    * might not be smallest"""
    a %= mod
    rem = mod - b % mod
    return extended_gcd(a, mod)[0] * rem


def main():
    coeff1, coeff2 = parametrize(SEQ)
    # print(coeff1, coeff2)

    init = int((N-coeff2) / coeff1) + 1

    # problem is now equivalent to solving the following equation
    # (coeff1 * x + coeff2) is integer and > N
    # solve modular equation with extended gcd
    # (a*x + b) % mod == 0

    # denominators of coeff1 and coeff2 have to be the same
    mod = coeff1.denominator
    base = mod_solve(coeff1.numerator, coeff2.numerator, mod)
    # smallest x, s.t.  x = base + k*mod > init
    x = (base + ((init - base) // mod + 1) * mod)
    print(x * coeff1 + coeff2)

    # solve for candidate with brute force, (fast enough)
    # for i in range(init, init*2):
    #     candidate = coeff1 * i + coeff2
    #     if candidate.denominator == 1 and candidate > N:
    #         print(i)
    #         print(candidate.numerator)
    #         break


if __name__ == '__main__':
    main()
