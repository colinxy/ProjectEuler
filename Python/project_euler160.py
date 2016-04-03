from math import log


def prod_mod_strip0(top, mod):
    pow2 = int(log(top, 2))
    twos = sum(top // (2 ** i) for i in range(1, pow2 + 1))
    pow5 = int(log(top, 5))
    fives = sum(top // (5 ** i) for i in range(1, pow5 + 1))
    # two_left = sum(top // (2 ** i) for i in range(1, pow2 + 1)) - \
    #     sum(top // (5 ** i) for i in range(1, pow5 + 1))

    print(pow2)
    print(twos)
    print(pow5)
    print(fives)

    prod_mod = 1
    reduce_twos = fives
    for i in range(1, top + 1):
        while reduce_twos > 0 and i % 2 == 0:
            i //= 2
            reduce_twos -= 1
        while i % 5 == 0:
            i //= 5

        i %= mod
        prod_mod = prod_mod * i % mod

    # prod_mod = prod_mod * pow(2, two_left, mod) % mod

    return prod_mod


def main():
    mod = 10 ** 5
    N = 10 ** 4

    prod_mod = prod_mod_strip0(N, mod)
    print(prod_mod)

    # print(pow(prod_mod, 10**7, mod))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
