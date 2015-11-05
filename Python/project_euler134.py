from mathutil import prime_under


def linear_mod(a, b, mod):
    """
    solve equation: a*x congruent to b MOD m  OR  a * x % m == b % m
    first solve a * x' % m == 1, x = x' * b
    """
    m_coeff, a_coeff = (1, 0), (0, 1)
    m = mod
    while a != 1:
        div = m // a
        m, a = a, m % a

        a_coeff_tmp = a_coeff
        a_coeff = m_coeff[0]-div*a_coeff[0], m_coeff[1]-div*a_coeff[1]
        m_coeff = a_coeff_tmp

    result = a_coeff[1] * b
    return result % mod


def main():
    N = 10 ** 6
    primes = prime_under(N + int(N ** 0.5))
    print("Primes generated:", time() - starting_time)

    sigma_s = 0
    for i in range(3, len(primes)):  # p1 starting from 5， p2 starting from 7
        p1 = primes[i-1]
        p2 = primes[i]
        if p1 > N:
            break

        last_digits = 10 ** len(str(p1))
        a = last_digits % p2
        b = p2 - p1
        curr_s = linear_mod(a, b, p2) * last_digits + p1
        # print(p1, p2, curr_s)
        sigma_s += curr_s

    print(sigma_s)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
