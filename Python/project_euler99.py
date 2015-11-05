# Largest exponential
"""
Comparing two numbers written in index form like 211 and 37 is not difficult, 
as any calculator would confirm that 211 = 2048 < 37 = 2187.

However, confirming that 632382518061 > 519432525806 would be much more 
difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text 
file containing one thousand lines with a base/exponent pair on each line, 
determine which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers 
in the example given above.
"""

from math import log
INF = 10**9


def main():
    with open("p099_base_exp.txt", 'r') as f:
        base_exp = [tuple(map(int, line.split(','))) for line in f.readlines()]
        print(max(zip(range(1, INF), base_exp), key=lambda i_b_e: 
                                           i_b_e[1][1] * log(i_b_e[1][0])))


if __name__ == '__main__':
    main()
