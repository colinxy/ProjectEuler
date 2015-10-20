# Truncatable primes
"""
The number 3797 has an interesting property. Being prime itself, 
it is possible to continuously remove digits from left
to right, and remain prime at each stage: 3797, 797, 97, and 7. 
Similarly we can work from right to left: 3797, 379, 37,
and 3.

Find the sum of the only eleven primes that are both truncatable 
from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
"""

from mathutil import is_prime, prime_under


def is_truncatable(x):
    s = str(x)
    for i in range(1, len(s)):
        if int(s[:i]) not in primes:
            return False
    for i in range(-1, -len(s), -1):
        if int(s[i:]) not in primes:
            return False
    return True

primes = set(prime_under(800000))


def main():
    truncatable = []
    for prime in primes:
        if is_truncatable(prime):
            truncatable.append(prime)
            if len(truncatable) == 11:
                break

    print(truncatable)
    print(len(truncatable))
    print(sum(truncatable))


if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
