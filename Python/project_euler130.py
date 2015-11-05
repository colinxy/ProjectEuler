from mathutil import is_prime
from project_euler129 import A


def main():
    N = 25

    composites = []
    for n in range(3, 1000000, 2):
        if n % 5 == 0 or is_prime(n):
            continue
        a_n = A(n)
        if (n-1) % a_n == 0:
            # print(n, a_n)
            composites.append(n)
            if len(composites) >= N:
                break
    print(sum(composites))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
