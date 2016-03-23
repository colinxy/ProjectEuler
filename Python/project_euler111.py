# project euler 111
# Primes with runs

from mathutil import miller_rabin


def digits9(digit):
    primes = []
    base = 1111111111

    # as first digit
    for i in range(1, 10):
        candidate = (base - 10 ** 9) * digit + i * 10 ** 9
        if miller_rabin(candidate):
            primes.append(candidate)

    # not as first digit
    for i in range(10):
        for d in range(9):
            candidate = (base - 10 ** d) * digit + i * 10 ** d
            if miller_rabin(candidate):
                primes.append(candidate)

    return primes


def digits9_for_evenor5(digit):
    primes = []
    for i in [1, 3, 7, 9]:
        candidate = 1111111110 * digit + i
        if miller_rabin(candidate):
            primes.append(candidate)

    return primes


def digits8_for_evenor5(digit):
    primes = []

    # as first digit
    for i in range(1, 10):
        for j in [1, 3, 7, 9]:
            candidate = i * 10 ** 9 + 111111110 * digit + j
            if miller_rabin(candidate):
                primes.append(candidate)

    # not as first digit
    bases = [1011111110, 1101111110, 1110111110, 1111011110,
             1111101110, 1111110110, 1111111010, 1111111100]
    for base in bases:
        for i in range(10):
            for j in [1, 3, 7, 9]:
                candidate = base * digit + (1111111110 - base) * i + j
                if miller_rabin(candidate):
                    primes.append(candidate)

    return primes


def main():
    primes = []

    # 0, X00000000X
    for i in range(1, 10):
        for j in [1, 3, 7, 9]:
            candidate = i * 10 ** 9 + j
            if miller_rabin(candidate):
                primes.append(candidate)

    # even digits

    # 2, 8 digits
    primes.extend(digits8_for_evenor5(2))
    # 4, 9 digits
    primes.extend(digits9_for_evenor5(4))
    # 6, 9 digits
    primes.extend(digits9_for_evenor5(6))
    # 8, 8 digits
    primes.extend(digits8_for_evenor5(8))

    # odd digits

    # 1, 9 digits
    primes.extend(digits9(1))
    # 3, 9 digits
    primes.extend(digits9(3))
    # 5, 9 digits
    primes.extend(digits9_for_evenor5(5))
    # 7, 9 digits
    primes.extend(digits9(7))
    # 9, 9 digits
    primes.extend(digits9(9))

    # print(primes)
    print(sum(primes))


if __name__ == '__main__':
    main()
