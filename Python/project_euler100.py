"""
red = m - n
blue = n

n   n-1   1
- * --- = -
m   m-1   2

(2m-1)**2 - 2(2n-1)**2 = -1

x = 2m-1
y = 2n-1
"""

import pell


def main():
    threshold = 10**12

    fund_x, fund_y = pell.pell(2, True)
    prev_x, prev_y = fund_x, fund_y
    m = (prev_x + 1) // 2
    n = (prev_y + 1) // 2

    while m < threshold:
        prev_x, prev_y = \
            pell.next_solution(2, fund_x, fund_y, prev_x, prev_y, True)
        m = (prev_x + 1) // 2
        n = (prev_y + 1) // 2

        # print(m-n, n)

    m = (prev_x + 1) // 2
    n = (prev_y + 1) // 2

    print(n)


if __name__ == '__main__':
    main()
