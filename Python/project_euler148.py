# Reference:
# https://en.wikipedia.org/wiki/Lucas%27s_theorem
# https://larryriddle.agnesscott.org/ifs/siertri/LucasProof.htm
#
# binomial coefficient nCr(m, n) is divisible by p iff
# at least one of the base p digits of n is greater than the corresponding digit of m
#
# alternatively, binomial coefficient nCr(m, n) is not divisible by p iff
# all base p digits of n is less than or equal to corresponding digit of m

from math import prod


def div7(num):
    count = 0
    while num % 7 == 0:
        count += 1
        num //= 7
    return count


# brute force
def div7_row(m):
    """number of numbers that is divisible by 7 in row m.
    row index starts from 0.
    """
    combination = m
    pow7 = 0
    dividend, divisor = combination, 1

    count = 0
    for i in range(1, (combination + 1) // 2):
        pow7 += div7(dividend) - div7(divisor)
        if pow7 > 0:
            # print(m, i)
            count += 1
        dividend -= 1
        divisor += 1

    count *= 2
    if combination % 2 == 0:
        pow7 += div7(dividend) - div7(divisor)
        if pow7 > 0:
            # print(m, combination // 2)
            count += 1

    return count


def not_div7_row(row_base7):
    # row_base7 is the row index written in base 7
    # find the number of entries not divisible by 7 in row
    return prod(i + 1 for i in row_base7)


def base7_add1(base7):
    """
    base7 is reversed: the least significant digit goes first
    """
    base7[0] += 1
    for d in range(len(base7)):
        if base7[d] < 7:
            break

        # base7[d] == 7
        # carry-over
        base7[d] = 0
        if d < len(base7) - 1:
            base7[d + 1] += 1
        else:
            base7.append(1)

    return base7


def not_div7_tri(n):
    count = 0
    # row number in base 7
    row_base7 = [0]
    for row in range(n):
        curr_count = not_div7_row(row_base7)
        count += curr_count

        row_base7 = base7_add1(row_base7)

    return count


def main():
    print(not_div7_tri(100))

    def tri(n):
        return (n+1)*n//2

    # find all m, n such that all base 7 digits of m is less than n
    N = 10 ** 9
    # 10**9-1 in base7: 33531600615

    total = 0
    # 1st digit 0-2
    total += tri(3) * tri(7)**10
    # 1st digit 3
    # 2nd digit 0-2
    total += 4 * tri(3) * tri(7)**9
    # 1st digit 3, 2nd digit 3
    # 3rd digit 0-4
    total += 4 * 4 * tri(5) * tri(7)**8
    # 1st digit 3, 2nd digit 3, 3rd digit 5
    # 4th digit 0-2
    total += 4 * 4 * 6 * tri(3) * tri(7)**7
    # 1st digit 3, 2nd digit 3, 3rd digit 5, 4th digit 3
    # 5th digit 0
    total += 4 * 4 * 6 * 4 * tri(1) * tri(7)**6
    # 1st digit 3, 2nd digit 3, 3rd digit 5, 4th digit 3, 5th digit 1
    # 6th digit 0-5
    total += 4 * 4 * 6 * 4 * 2 * tri(6) * tri(7)**5
    # 1st digit 3, 2nd digit 3, 3rd digit 5, 4th digit 3, 5th digit 1, 6th digit 6, 7th digit 0, 8th digit 0
    # 9th digit 0-5
    total += 4 * 4 * 6 * 4 * 2 * 7 * tri(6) * tri(7)**2
    # 1st digit 3, 2nd digit 3, 3rd digit 5, 4th digit 3, 5th digit 1, 6th digit 6, 7th digit 0, 8th digit 0, 9th digit 6
    # 10th digit 0
    total += 4 * 4 * 6 * 4 * 2 * 7 * 7 * tri(1) * tri(7)**1
    # 1st digit 3, 2nd digit 3, 3rd digit 5, 4th digit 3, 5th digit 1, 6th digit 6, 7th digit 0, 8th digit 0, 9th digit 6, 10th digit 1
    # 11th digit 0-5
    total += 4 * 4 * 6 * 4 * 2 * 7 * 7 * 2 * tri(6)

    print(total)
    # can be solved in the general case rather than manually


if __name__ == '__main__':
    main()
