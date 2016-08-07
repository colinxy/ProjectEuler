# project euler 348: Sum of a square and a cube

from __future__ import division

TOP = 10 ** 9


def is_palindrome(n):
    return str(n) == str(n)[::-1]


def main():
    sum_cache = {}
    for i in range(2, int(TOP**0.5)):
        square = i * i
        for j in range(2, int(TOP**(1/3))):
            cube = j * j * j
            sq_cb = square + cube
            if sq_cb >= TOP:
                break
            sum_cache[sq_cb] = sum_cache.get(sq_cb, 0) + 1

    nums = [sq_cb for sq_cb in sum_cache
            if sum_cache[sq_cb] == 4 and is_palindrome(sq_cb)]
    nums.sort()
    # print(nums)
    # print(len(nums))
    print(sum(nums[:5]))


if __name__ == '__main__':
    main()
