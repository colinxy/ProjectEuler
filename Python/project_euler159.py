# project euler 159: Digital root sums of factorisations

from __future__ import division
from math import sqrt

N = 10**6


def digit_root(n):
    return 1 + (n-1) % 9
    # dr = sum(map(int, str(n))) % 9
    # return 9 if dr == 0 else dr

digit_root_cache = [0, 1] + [digit_root(i) for i in range(2, N)]

# has to be populated sequentially, from 2 to N
max_digit_roo_sum_cache = [0, 0]


# has to be called in sequence, from 2 to N
def max_digital_root_sum(num):
    # comprehension version
    # dr_sums = [digit_root_cache[i]+max_digit_roo_sum_cache[num//i]
    #            for i in range(2, int(sqrt(num))+1) if num % i == 0]
    # max_sum = max(dr_sums or [0])
    # max_sum = max(max_sum, digit_root_cache[num])

    # loop version
    max_sum = digit_root_cache[num]
    for i in range(2, int(sqrt(num))+1):
        if num % i == 0:
            dr_sum = digit_root_cache[i]+max_digit_roo_sum_cache[num//i]
            if dr_sum > max_sum:
                max_sum = dr_sum

    # fill cache
    max_digit_roo_sum_cache.append(max_sum)
    return max_sum


def main():
    # starts from 2
    print(sum(max_digital_root_sum(i) for i in range(2, N)))


if __name__ == '__main__':
    main()
