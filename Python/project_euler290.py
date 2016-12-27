
from __future__ import print_function

DIGITS = 18
N = 10 ** DIGITS

ZERO = ord('0')


def dsum(num):
    return sum(ord(c)-ZERO for c in str(num))


def digit_sig(ndigits):
    def add_digit(carry_over, dsum_diff, left_digits):
        if (carry_over, dsum_diff, left_digits) in cache:
            return cache[(carry_over, dsum_diff, left_digits)]

        if left_digits == 0:
            return dsum(carry_over) == dsum_diff
        total = 0
        for i in range(10):
            carried = carry_over + 137 * i
            new_carry_over, d = divmod(carried, 10)
            new_dsum_diff = dsum_diff + i - d
            total += add_digit(new_carry_over, new_dsum_diff, left_digits-1)

        cache[(carry_over, dsum_diff, left_digits)] = total
        return total

    cache = {}
    return add_digit(0, 0, ndigits)


def main():
    # total = 0
    # for i in range(0, N, 9):
    #     # print(i, dsum(i), i*137, dsum(i*137))
    #     if dsum(i) == dsum(i*137):
    #         total += 1
    # print(total)

    print(digit_sig(DIGITS))


if __name__ == '__main__':
    main()
