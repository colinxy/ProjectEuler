from math import sqrt, ceil

from mathutil import prime_under


def is_prime_range(start, end, primes=None):
    """
    Find all primes in [start, end)

    Using sieve with offset
    """
    if not primes:
        primes = prime_under(int(sqrt(end))+1)

    is_prime_offset = [True] * (end-start)
    for p in primes:
        if p * p >= end:
            break
        for j in range(int(ceil(start/p)*p), end, p):
            is_prime_offset[j-start] = False

    return is_prime_offset


def row_bound(row_n):
    start = row_n * (row_n-1) // 2 + 1
    end = (row_n+1) * row_n // 2
    return start, end


def S(n, primes=None):
    ### total considered range: row n-2 to row n+2
    start, _ = row_bound(n-2)
    _, end = row_bound(n+2)

    ### row n
    row_n_start, row_n_end = row_bound(n)

    ### row n-1
    row_prev_start = (n-1) * (n-2) // 2 + 1
    row_prev_end = n * (n-1) // 2
    ### row n+1
    row_next_start = (n+1) * n // 2 + 1
    row_next_end = (n+2) * (n+1) // 2

    is_prime_offset = is_prime_range(start, end, primes=primes)

    def is_prime(num):
        return is_prime_offset[num-start]

    def neighbors(num, row_n):
        # no need to consider row end because it is not prime
        row_n_start, row_n_end = row_bound(n)

        hood = [
            num-row_n,   num-row_n+1, num-row_n+2,
            num-1,                    num+1,
            num+row_n-1, num+row_n,   num+row_n+1,
        ]
        row_ns = [
            row_n-1, row_n-1, row_n-1,
            row_n,            row_n,
            row_n+1, row_n+1, row_n+1,
        ]
        if num == row_n_start:
            hood = [
                num-row_n+1, num-row_n+2,
                             num+1,
                num+row_n,   num+row_n+1,
            ]
            row_ns = [
                row_n-1, row_n-1,
                         row_n,
                row_n+1, row_n+1,
            ]
        elif num == row_n_end-1:
            hood = [
                num-row_n,   num-row_n+1,
                num-1,                    num+1,
                num+row_n-1, num+row_n,   num+row_n+1,
            ]
            row_ns = [
                row_n-1, row_n-1,
                row_n,            row_n,
                row_n+1, row_n+1, row_n+1,
            ]
        return hood, row_ns

    def is_triplet_center(num, row_n):
        """
        num is prime and connects at least 2 other primes
        """
        if not is_prime(num):
            return False
        hood, _ = neighbors(num, row_n)
        prime_hood = [is_prime(h) for h in hood]
        return sum(prime_hood) >= 2

    triplet_elem = []
    for i in range(*row_bound(n)):
        if not is_prime(i):
            continue

        if is_triplet_center(i, n):
            triplet_elem.append(i)
            continue

        hood, row_ns = neighbors(i, n)
        for num, row_n in zip(hood, row_ns):
            if is_triplet_center(num, row_n):
                triplet_elem.append(i)
                break

    return triplet_elem


def S_k(*ns):
    ns = sorted(ns, reverse=True)

    _, upper_bound = row_bound(ns[0]+2)
    primes = prime_under(int(sqrt(upper_bound))+1)

    sums = 0
    for n in ns:
        s = S(n, primes=primes)
        print(len(s), sum(s))
        sums += sum(s)
    return sums


if __name__ == '__main__':
    print(S_k(5678027, 7208785))
