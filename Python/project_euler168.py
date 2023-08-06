# 1 <= b <= 9, 1 <= c <= 9
# _
# ab = 10a + b
#  _
# ba = a + b 10^k
#
# right-rotation relationship: let c be the factor
# c(10a + b) = a + b 10^k
# a(10c - 1) = b (10^k - c)
#
# Consider the relationship between (10c - 1) and (10^k - c)
# also make use of Fermat's little theorem / Euler's theorem

CEILING = 100

## c = 1
# all digits of a and b must be the same
# e.g. a = 11, b = 1
def c1_mod_sum():
    # 2-5 digits
    nums = [11, 111, 1111, 11111]
    mod_sum2_5 = sum(nums) * sum(range(1, 10)) % (10**5)
    # 6+ digits
    mod_sum6_up = (CEILING - 5) * 11111 * sum(range(1, 10)) % (10**5)

    return (mod_sum2_5 + mod_sum6_up) % (10**5)


### NOTE: experimental attempt at c = 2, not used for final solution
#
## c = 2
# 19a = b (10^k - 2)
# 10^k === 2 (mod 19)  then we can apply Fermat's little theorem
# k === 17 (mod 18)
#
# let k = 17
# (10**17-2)/19 = 5263157894736842, let m be this number
# m has 16 digits, we need 17 (k = 17)
# a = b * m
# b = 2 .. 9
def c2():
    special_nums = []

    for k in range(17, CEILING, 18):
        m = (10**k - 2) // 19
        for b in range(1, 10):
            a = b * m
            if a < 10**(k-1):
                continue
            if a >= 10**(k+1):
                break

            num = 10*a + b
            special_nums.append(num)

    return special_nums


def exp_mod_inverse(n, p):
    """
    Solve for the smallest x that satisfy:
    n^x === 1 (mod p)
    Once we find such x, all multiples of x are solutions

    Precondition: p prime, n and p are coprime

    Currently using brute force search

    NOTE:
    x = p-1 is a solution by Fermat's little theorem,
    but may not be the only solution under p-1
    (This is why my first attempt failed)
    """
    for x in range(1, p):
        if pow(n, x, p) == 1:
            return x


# from c = 2, we can generalize it:
# a(10c - 1) = b (10^k - c)
# given the precondition that 10c-1 is prime and 10c-1 > 10
# 10c-1 | 10^k - c
#
# when 10c - 1 is not prime,
# see below c = 4, c = 5, c = 7 for additional considerations
#
def c_prime(c):
    """
    Precondition: 10c-1 is prime
    """
    divisor = 10*c - 1
    # find solution to:
    # 10^k === c (mod 10c-1)
    # multiply both sides by 10
    # 10^(k+1) === 10c === 1 (mod 10c-1)
    k_mod = exp_mod_inverse(10, divisor)
    k_base = k_mod-1

    special_nums = []

    for k in range(k_base, CEILING, k_mod):
        m = (10**k - c) // divisor
        for b in range(1, 10):
            a = b * m
            if a < 10**(k-1):
                continue
            if a >= 10**(k+1):
                break

            num = 10*a + b
            # verify num is correct, should never break!
            if c * (10*a + b) != (a + b * 10**k):
                raise Exception('Broken! a={}, b={}, c={}'.format(a, b, c))

            special_nums.append(num)

    return special_nums


def c_try_prime(c, divisor, k_range):
    special_nums = []

    for k in k_range:
        for b in range(1, 10):
            a, mod = divmod((10**k - c) * b, divisor)
            if mod != 0:
                continue
            if a < 10**(k-1):
                continue
            if a >= 10**(k+1):
                break

            num = 10*a + b
            # verify num is correct, should never break!
            if c * (10*a + b) != (a + b * 10**k):
                raise Exception('Broken! a={}, b={}, c={}'.format(a, b, c))

            special_nums.append(num)

    return special_nums


## c = 4
# 39a = b (10^k - 4)
# 13 | 10^k - 4
# 10^(k+1) === 4*10 === 1 (mod 13)
def c4():
    c = 4
    k_mod = exp_mod_inverse(10, 13)
    return c_try_prime(c, 10*c-1, range(k_mod-1, CEILING, k_mod))


## c = 5
# 49a = b (10^k - 5)
# 7 | 10^k - 5
# 10^(k+1) === 5*10 === 1 (mod 7)
def c5():
    c = 5
    k_mod = exp_mod_inverse(10, 7)
    return c_try_prime(c, 10*c-1, range(k_mod-1, CEILING, k_mod))


## c = 7
# 69a = b (10^k - 7)
# 23 | 10^k - 7
# 10^(k+1) === 7*10 === 1 (mod 23)
def c7():
    c = 7
    k_mod = exp_mod_inverse(10, 23)
    return c_try_prime(c, 10*c-1, range(k_mod-1, CEILING, k_mod))


def main():
    # c = 1
    total = sum([
        c1_mod_sum(),
        sum(c_prime(2)),
        sum(c_prime(3)),
        sum(c4()),
        sum(c5()),
        sum(c_prime(6)),
        sum(c7()),
        sum(c_prime(8)),
        sum(c_prime(9)),
    ]) % (10**5)

    print(total)


if __name__ == '__main__':
    main()
