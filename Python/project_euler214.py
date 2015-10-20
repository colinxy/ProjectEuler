from mathutil import prime_factorization, prime_factors_under_lazy_dict


def totient(n, factors=None):
    if factors is None:
        factors = prime_factorization(n).keys()
    for fac in factors:
        n //= fac
        n *= fac - 1
    return n


def main():
    N = 4 * 10 ** 7
    given_len = 25

    totient_cache = [0] * (N+1)
    totient_cache[2] = 1
    length_cache = [0] * (N+1)
    length_cache[2] = 2
    p_file = open("primes.txt")
    primes = map(int, p_file.read().splitlines())
    p_file.close()
    print("all primes loaded:", time() - starting_time, "seconds")

    ps = []
    for p in primes:
        if p > N:
            break
        length = 2
        tot = p - 1

        to_update = [p-1]
        while tot != 1:
            if length_cache[tot] != 0:
                length += length_cache[tot]
                break

            to_update.append(tot)

            if totient_cache[tot] != 0:
                tot = totient_cache[tot]
            else:
                totient_cache[tot] = totient(tot)
                tot = totient_cache[tot]

            length += 1

        l = length
        for i in to_update:
            l -= 1
            length_cache[i] = l

        # print(p, length)
        if length == given_len:
            # print(p)
            ps.append(p)

    # print(ps)
    print(sum(ps))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # slow, 5 min
    print("Time elapsed:", time() - starting_time, "seconds")
