from mathutil import prime_under
from project_euler132 import repunit_mod


def main():
    N = 10 ** 5

    # heuristic and slow!
    print("\nheuristic and slow!")
    D = 50  # probe depth
    primes = prime_under(N)
    ever = []
    for p in primes[3:]:
        if any(repunit_mod(p, i) == 0 for i in range(1, D+1)):
            # print(p)
            ever.append(p)
            continue
    print(ever)
    print(sum(primes) - sum(ever))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
