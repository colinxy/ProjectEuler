from mathutil import prime_factorization


def print_factorization(factorization):
    factors = sorted(factorization.keys())
    print(" * ".join("{}^{}".format(fac, factorization[fac]) for fac in factors))


def main():
    number = 100
    while True:
        factorization = prime_factorization(number * (number - 1) // 2)
        divisors = 1
        for i in factorization.values():
            divisors *= i + 1
        if divisors > 500:
            print(number * (number - 1) // 2)
            break

        number += 1


if __name__ == '__main__':
    from time import time
    starting_time = time()
    print_factorization(prime_factorization(1234567890))
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
