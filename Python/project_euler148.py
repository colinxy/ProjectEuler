from math import log, floor, ceil
from mathutil import product


def div7(num):
    count = 0
    while num % 7 == 0:
        count += 1
        num //= 7
    return count


# brute force
def div7_row(m):
    """number of numbers that is divisible by 7 in row m.
    m is (the actual row in Pascal Triangle - 1)
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

SIZE = 10 ** 9
# 10 ** 9
# 3     3     5    3    1    6    0    0    6    1    6
# 7**11 7**10 7**9 7**8 7**7 7**6 7**5 7**4 7**3 7**2 7


def main():
    # brute firce
    # count = 0
    # for i in range(1, SIZE):
    #     count += div7_row(i)
    # print(count)
    # print(SIZE * (SIZE + 1) // 2 - count)

    # POW = ceil(log(SIZE, 7))
    # pow7 = [7**i for i in range(1, POW)]
    # triangle = [(i - 1) * i // 2 for i in pow7]
    # whole_triangle = [triangle[0]]
    # for i in range(1, POW - 1):
    #     whole_triangle.append(whole_triangle[-1] * 21 * 3 + triangle[i])

    # print(pow7)
    # print(triangle)
    # print(whole_triangle)

    # # top level
    # covered = (7 ** (POW - 1) * 2)
    # total = whole_triangle[-1] * (SIZE // covered)

    # print(total)
    # print(SIZE - covered)

    # by Lucus's Thm
    count = 0
    row_base7 = [0]
    for row in range(0, SIZE):
        count += product(i + 1 for i in row_base7)

        row_base7[0] += 1
        for d in range(len(row_base7)):
            if row_base7[d] == 7:
                row_base7[d] = 0
                if d < len(row_base7) - 1:
                    row_base7[d + 1] += 1
                else:
                    row_base7.append(1)
                    break
            else:
                break

    print(count)


if __name__ == '__main__':
    main()
