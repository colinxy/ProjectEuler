
def prime_under(ceiling):
    primes = []
    primality = [True] * ceiling
    primality[0], primality[1] = False, False
    for (i, isPrime) in enumerate(primality):
        if isPrime:
            primes.append(i)
            for j in range(i * i, ceiling, i):
                primality[j] = False

    return primes


def squares(lower, upper):
    return [i**2 for i in range(int(lower**0.5), int(upper**0.5)+1)]


def main():
    prime_cube = []
    last = 1
    for p in prime_under(1000000)[2:]:
        for k in squares(last, p // 3 + 1):
            delta = (k*(4*p-3*k))**0.5 * k
            if delta.is_integer():
                n = (3*k*k+delta) / (2*(p-3*k))
                if n.is_integer():
                    print(p, n, k)
                    prime_cube.append((p, n))
                    last = k
                    break

    print(len(prime_cube))


if __name__ == '__main__':
    from time import time
    start = time()
    main()
    print("running time:", time()-start, "seconds")
