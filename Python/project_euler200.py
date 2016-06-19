# project euler 200:
# Find the 200th prime-proof sqube containing the contiguous sub-string "200"

from itertools import islice
from mathutil import prime_under, miller_rabin

MAX = 10 ** 12


def prime_proof(n):
    n_in_digits = list(str(n))

    # first digit cannot be 0
    digit = n_in_digits[0]
    for d in range(1, 10):
        n_in_digits[0] = str(d)
        if miller_rabin(int(''.join(n_in_digits))):
            return False
    n_in_digits[0] = digit

    # latter digits
    for i in range(1, len(n_in_digits)):
        digit = n_in_digits[i]
        for d in range(10):
            n_in_digits[i] = str(d)
            if miller_rabin(int(''.join(n_in_digits))):
                return False
        n_in_digits[i] = digit

    return True


def main():
    squbes200 = []
    primes = prime_under(int(MAX ** 0.5))
    prime_len = len(primes)

    for i in range(prime_len):
        if primes[i] ** 3 > MAX:
            break
        for j in range(i+1, prime_len):
            sqube = primes[i] ** 3 * primes[j] ** 2
            if sqube > MAX:
                break
            if '200' in str(sqube):
                squbes200.append(sqube)
            sqube = primes[i] ** 2 * primes[j] ** 3
            if '200' in str(sqube) and sqube < MAX:
                squbes200.append(sqube)

    squbes200 = sorted(filter(lambda a: a < MAX, squbes200))
    # print(squbes200[-1])
    # print(len(squbes200))

    print(list(islice((s for s in squbes200 if prime_proof(s)), 200))[-1])

    # count = 0
    # for s in squbes200:
    #     if prime_proof(s):
    #         count += 1
    #         if count == 200:
    #             print(s)
    #             break


if __name__ == '__main__':
    main()
