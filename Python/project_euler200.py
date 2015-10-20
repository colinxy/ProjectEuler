from mathutil import prime_under, is_prime_array


def main():
    MAX = 1000000000
    primes = prime_under(1000000)
    print("primes acquired", time()-starting_time)

    squbes200 = []
    prime_len = len(primes)

    for i in range(prime_len):
        for j in range(i+1, prime_len):
            sqube = primes[i] ** 3 * primes[j] ** 2
            if sqube > MAX:
                break
            if '200' in str(sqube):
                squbes200.append(sqube)
            sqube = primes[i] ** 2 * primes[j] ** 3
            if '200' in str(sqube):
                squbes200.append(sqube)

    squbes200 = sorted(filter(lambda a: a < MAX, squbes200))
    print(squbes200[-1])
    print(len(squbes200))

    is_prime_arr = is_prime_array(MAX)
    print("is prime array generated", time()-starting_time)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
