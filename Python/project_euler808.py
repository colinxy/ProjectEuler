from math import isqrt
from mathutil import prime_under, is_prime


def is_rps(n):
    n_rev = int(str(n)[::-1])
    if n_rev == n:
        return False

    sqrt_n_rev = isqrt(n_rev)
    if sqrt_n_rev*sqrt_n_rev != n_rev:
        return False

    return is_prime(sqrt_n_rev)


def main(primes, n):
    rps = []
    for p in primes:
        p_sq = p*p
        if is_rps(p_sq):
            rps.append(p_sq)
            print(len(rps), p, p_sq)
        if len(rps) == n:
            break

    print(sum(rps))


if __name__ == '__main__':
    main(prime_under(4*10**7), 50)
