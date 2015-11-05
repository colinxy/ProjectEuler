from math import log
from mathutil import prime_under


def m(p1, p2, n):
    assert p1 < p2
    prod = p1 * p2
    if prod > n:
        return 0

    p1_extra = int(log(n / prod, p1))
    prod *= p1 ** p1_extra
    max_prod = prod

    while p1_extra > 0:
        prod //= p1
        p1_extra -= 1
        if prod * p2 <= n:
            prod *= p2
            if prod > max_prod:
                max_prod = prod

    return max_prod


def main():
    # print(m(2, 3, 1000))

    N = 10000000
    m_vals = []
    primes = prime_under(N // 2 + 1)
    primes_len = len(primes)
    print("Primes computed by", time() - starting_time)

    for i in range(primes_len):
        if primes[i] * primes[i] > N:
            break
        for j in range(i + 1, primes_len):
            if primes[i] * primes[j] > N:
                break
            m_curr = m(primes[i], primes[j], N)
            # print(primes[i], primes[j], m_curr)
            m_vals.append(m_curr)

    print(sum(m_vals))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time() - starting_time, "seconds")
