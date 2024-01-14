from mathutil import miller_rabin

START = 10**14
MOD = 1234567891011


def next_primes(start, count):
    primes = []
    for i in range(start+1, 2*start):
        if miller_rabin(i):
            primes.append(i)
            if len(primes) >= count:
                break

    return primes


def fib(n):
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a+b

    return a


def mat_mul(a, b):
    a0, a1, a2, a3 = a
    b0, b1, b2, b3 = b
    return [
        (a0*b0 + a1*b2) % MOD,
        (a0*b1 + a1*b3) % MOD,
        (a2*b0 + a3*b2) % MOD,
        (a2*b1 + a3*b3) % MOD,
    ]


def fib_matrix_exp_mod(n):
    """
    This function finds
    ( 1  1 )^n
    (      )
    ( 1  0 )

    Usage notes:

    ( F2 )   ( 1  1 ) ( F1 )
    (    ) = (      ) (    )
    ( F1 )   ( 1  0 ) ( F0 )

    where F0 = 0, F1 = 1
    then it follows that

    ( F(n)   )   ( 1  1 )^(n-1) ( F1 )
    (        ) = (      )       (    )
    ( F(n-1) )   ( 1  0 )       ( F0 )

    We can therefore find F(n) in log(n) time
    """

    base = [1, 1, 1, 0]

    if n == 0:
        return [1, 0, 0, 1]     # identity
    if n == 1:
        return base

    half_exp = fib_matrix_exp_mod(n >> 1)
    full_exp = mat_mul(half_exp, half_exp)
    if n & 1:
        return mat_mul(full_exp, base)
    return full_exp


def fib_incr(nums):
    """
    Precondition: nums is sorted increasing.

    Best use when nums[0] is large, and each increment is small
    """
    fibs = []

    matrix = fib_matrix_exp_mod(nums[0]-1)
    fibs.append(matrix[0])
    for i in range(1, len(nums)):
        incr = nums[i] - nums[i-1]
        matrix = mat_mul(fib_matrix_exp_mod(incr), matrix)
        fibs.append(matrix[0])

    return fibs


def main():
    primes = next_primes(START, 10**5)
    print('smallest', primes[0], 'largest', primes[-1])

    print(sum(fib_incr(primes)) % MOD)


if __name__ == '__main__':
    main()
