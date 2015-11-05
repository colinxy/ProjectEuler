from math import log


def prod_mod_strip0(top, mod):
    twos = int(log(top, 2))
    fives = int(log(top, 5))
    two_left = sum(top // (2 ** i) for i in range(1, twos + 1)) - sum(top // (5 ** i) for i in range(1, fives + 1))

    prod_mod = 1
    for i in range(1, top + 1):
        while i % 2 == 0:
            i //= 2
        while i % 5 == 0:
            i //= 5
        prod_mod = prod_mod * i % mod

    prod_mod = prod_mod * pow(2, two_left, mod) % mod

    return prod_mod


def main():
    mod = 10 ** 5
    N = 10 ** 5

    prod_mod = prod_mod_strip0(N, mod)
    print(prod_mod)

    print(pow(prod_mod, 10**7, mod))
    print("wrong answer")


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
