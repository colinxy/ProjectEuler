# Quadratic primes
"""
Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the 
consecutive values n = 0 to 39. However, when n = 40, 
402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and 
certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 
80 primes for the consecutive values n = 0 to 79.
The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4
Find the product of the coefficients, a and b, for the 
quadratic expression that produces the maximum number of primes
for consecutive values of n, starting with n = 0.
"""

from time import time


def is_prime(n):
    if n <= 3:
        return n >= 2
    if n % 2 == 0 or n % 3 == 0:
        return False
    for i in range(5, int(n**0.5) + 1, 6):
        if n % i == 0 or n % (i+2) == 0:
            return False
    return True


def consecutive(a, b):
    n = 1
    expr = lambda x: x*x + a*x + b
    while is_prime(expr(n)):
        n += 1

    return n - 1


def main():
    prime_under1000 = [i for i in range(3, 1000) if is_prime(i)]
    
    # max_count = 0
    # max_pair = (0, 0)
    # for b in prime_under1000:
    #     for a in range(-999, 999, 2):
    #         current_count = consecutive(a, b)
    #         if current_count > max_count:
    #             max_count = current_count
    #             max_pair = (a, b)

    # print(max_count, max_pair)
    # print(max_pair[0] * max_pair[1])
    
    print(max((consecutive(a, b), a*b) for a in range(-999, 999, 2) for b in prime_under1000))


if __name__ == "__main__":
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
