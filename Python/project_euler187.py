# Semiprimes
"""
A composite is a number containing at least two prime factors. For example, 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.

There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors:
4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

How many composite integers, n < 108, have precisely two, not necessarily distinct, prime factors?
"""


def prime_under(ceiling):
    primality = [True] * ceiling
    primality[0], primality[1] = False, False
    for (i, isPrime) in enumerate(primality):
        if isPrime:
            yield i
            for j in range(i * i, ceiling, i):
                primality[j] = False


def main():
    ceiling = 100000000
    total = 0
    it_prs = list(prime_under(int(ceiling**0.5)+1))
    prs = list(prime_under(ceiling//2))
    length = len(prs)
    print("primes computed by:", time()-starting_time, "s")

    i = -1
    for index, prime in enumerate(it_prs):
        for i in range(i, -length, -1):
            if prs[i] * prime < ceiling:
                break
        this = length + i - index + 1
        total += this
        # print(prime, prs[i], this)
    print(total)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "s")
