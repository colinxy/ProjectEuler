### NOTE:
# rewritten in golang to run faster, see Golang/pe810.go
#
# not needed but cool functions:
# 1. xor_divmod: long division
# 2. try_factor: basic attempt to find 2 factors of the number

# properties of xor_product
#
# 1. let z = xor_product(x, y), then z >= x and z >= y
# 2. xor_product(x, y) == xor_product(y, x)
# 3. xor_product(xor_product(x, y), z) == xor_product(x, xor_product(y, z))
# 4. let z = xor_product(x, y), then bit_count(z) = bit_count(x) * bit_count(y) + 2k
#
# observations
#
# 1. bit_count(x) even => x divisible by 0b11
# 2. let z = xor_product(x, y), LSB(z) = 1 at 0 index
# 3. let z = xor_product(x, y), MSB(z) = 1, index of MSB is sum of x and y's MSB
#
def xor_product(x, y):
    product = 0

    exp = 1
    while exp <= x:
        product ^= (x & exp) * y
        exp <<= 1

    return product


def msb(x):
    """Index of Most Significant Bit

    Precondition: x > 0
    """
    i = 0
    while x > 1:
        x >>= 1
        i += 1
    return i


def xor_divmod(x, y):
    """Simulation of long division

    Precondition: x >= y
    """
    x_msb = msb(x)
    y_msb = msb(y)
    # shift y to have the same MSB as x
    y_shift = y << (x_msb - y_msb)

    div = 0
    while y_shift >= y:
        div <<= 1
        if msb(x) == msb(y_shift):
            div += 1
            x ^= y_shift
        y_shift >>= 1

    return div, x


def xor_sieve(n):
    """
    2, 3, 7, 11, 13, 19, 25, 31, 37, 41, ...
    """
    # is_xor_prime won't be fully populated due to optimizations,
    # e.g. range(5, n, 2), i.bit_count()
    is_xor_prime = [False] * n
    is_xor_prime[2], is_xor_prime[3] = True, True
    for i in range(5, n, 2):
        # potential xor-primes: odd bit_count
        is_xor_prime[i] = (i.bit_count() % 2 == 1)

    n_msb = msb(n)

    for i in range(5, n, 2):
        if not is_xor_prime[i]:
            continue

        for j in range(i, n):
            prod = xor_product(i, j)
            if prod < n:
                is_xor_prime[prod] = False

            if msb(prod) > n_msb:
                break

    return [i for i in range(n) if is_xor_prime[i]]


p1000 = xor_sieve(1000)


def try_factor(x):
    for p in p1000:
        div, mod = xor_divmod(x, p)
        if mod == 0:
            return p, div
