import math
from math import log
from mathutil import gcd


def terminate(numerator, denominator):
    left = denominator // gcd(numerator, denominator)
    while left % 2 == 0:
        left //= 2
    while left % 5 == 0:
        left //= 5
    if left == 1:  # terminate
        return -1
    else:          # non-terminate
        return 1


def main():
    total = 0
    for num in range(5, 10001):
        # maximize at N/e
        max_parts1 = num // math.e
        max_parts2 = num // math.e + 1
        max_prod, max_parts = max((log(num/parts) * parts, parts) for parts in [max_parts1, max_parts2])
        # print(num, max_parts)
        total += num * terminate(num, max_parts)
    print(total)


if __name__ == '__main__':
    from time import time

    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
