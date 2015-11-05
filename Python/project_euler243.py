# Resilience
"""
A positive fraction whose numerator is less than its denominator is called a proper fraction.
For any denominator, d, there will be d−1 proper fractions; for example, with d = 12:
1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

We shall call a fraction that cannot be cancelled down a resilient fraction.
Furthermore we shall define the resilience of a denominator, R(d), to be the ratio of its proper fractions that are
resilient; for example, R(12) = 4/11 .
In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .

Find the smallest denominator d, having a resilience R(d) < 15499/94744 .
"""

from functools import reduce
from operator import mul


def prime_under(ceiling):
    primes = []
    primality = [True] * ceiling
    primality[0], primality[1] = 0, 0
    for (i, isPrime) in enumerate(primality):
        if isPrime:
            primes.append(i)
            for j in range(i * i, ceiling, i):
                primality[j] = False

    return primes


def prime_factorization(n):
    result = {}

    if n % 2 == 0:
        result[2] = 1
        n //= 2
        while n % 2 == 0:
            result[2] += 1
            n //= 2

    factor = 3
    while factor <= n:
        if n % factor == 0:
            result[factor] = 1
            n //= factor
            while n % factor == 0:
                result[factor] += 1
                n //= factor
        factor += 2

    return result


# slow totient
def resilience(denominator):
    top = denominator // 2 + 1
    proper = [True] * top
    primes = list(prime_factorization(denominator).keys())
    # if is_prime(n): return n -1
    if len(primes) == 1:
        return denominator - 1
    for i in primes:
        proper[i] = False
        for j in range(i * i, top, i):
            proper[j] = False
    return len([i for i in proper[1:] if i]) * 2 + denominator % 2


def totient(n):
    tot = n
    primes = prime_factorization(n).keys()
    for i in primes:
        tot //= i
        tot *= (i - 1)
    return tot


def product(nums):
    return reduce(mul, nums)


def main():
    resilience_d = 15499 / 94744
    print(totient(6))
    print(totient(12))
    print(totient(30))
    print(totient(210))
    print(totient(product(prime_under(24))))

    lower_bound = product(prime_under(24))  # [2, 3, 5, 7, 11, 13, 17, 19, 23]
    upper_bound = product(prime_under(30))  # 29
    for i in range(lower_bound, upper_bound, lower_bound):  # heuristic !!!
        if totient(i) / (i-1) < resilience_d:
            print(i)
            break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
