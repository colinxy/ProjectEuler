# Odd period square roots
"""
All square roots are periodic when written as continued fractions 
and can be written in the form: It can be seen that the sequence 
is repeating. For conciseness, we use the notation 
√23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

The first ten continued fraction representations of 
(irrational) square roots are:

√2=[1;(2)], period=1
√3=[1;(1,2)], period=2
√5=[2;(4)], period=1
√6=[2;(2,4)], period=2
√7=[2;(1,1,1,4)], period=4
√8=[2;(1,4)], period=2
√10=[3;(6)], period=1
√11=[3;(3,6)], period=2
√12= [3;(2,6)], period=2
√13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?
"""


def is_perfect_square(n):
    if int(n ** 0.5) ** 2 == n or (int(n ** 0.5) + 1) ** 2 == n:
        return True
    else:
        return False


def gcd(x, y):
    assert x > 0 and y > 0
    if max(x, y) % min(x, y) == 0:
        return min(x, y)
    else:
        return gcd(min(x, y), max(x, y) % min(x, y))


def continued_fraction_period(n):  # precondition n is not a perfect square
    sequence = []
    # (x**0.5 + y) / z
    expr, cache_x, cache_y, cache_z = next_continued_fraction(n, -int(n ** 0.5))
    sequence.append(expr)
    x, y, z = cache_x, cache_y, cache_z

    while True:
        expr, x, y, z = next_continued_fraction(x, y, z)
        if (x, y, z) == (cache_x, cache_y, cache_z):
            return sequence
        sequence.append(expr)


# w * (x**0.5 + y) / z  =>  z * (x**0.5 - y) / (w * (x - y**2))
def next_continued_fraction(x, y, z=1, w=1):
    expr = int(z * (x ** 0.5 - y) / (w * (x - y ** 2)))
    # numerator: z * (x**0.5 - y)
    # denominator: w * (x - y**2)
    common_divisor = gcd(z, w * (x - y ** 2))
    w = w * (x - y ** 2) // common_divisor
    z //= common_divisor
    y = -y - expr * w
    if z != 1:
        print('z != 1', x, y, z, w)
    return expr, x, y, w


def main():
    count = 0
    for i in range(2, 10001):
        if not is_perfect_square(i):
            cf_period = continued_fraction_period(i)
            # print(cf_period)
            if len(cf_period) % 2 == 1:
                count += 1
    print(count)


if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
