# Coin sums

"""
In England the currency is made up of pound, £, and pence, p, 
and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
"""

from collections import defaultdict

COINS = [1, 2, 5, 10, 20, 50, 100, 200]


def main():
    count = 0
    this = 200
    for a in range(0, 200 // 200 + 1):
        this1 = this - a * 200
        for b in range(0, this1 // 100 + 1):
            this2 = this1 - b * 100
            for c in range(0, this2 // 50 + 1):
                this3 = this2 - c * 50
                for d in range(0, this3 // 20 + 1):
                    this4 = this3 - d * 20
                    for e in range(0, this4 // 10 + 1):
                        this5 = this4 - e * 10
                        for f in range(0, this5 // 5 + 1):
                            this6 = this5 - f * 5
                            count += 1 + this6 // 2

    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time)
