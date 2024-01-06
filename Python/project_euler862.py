from functools import cache

from mathutil import nCr


@cache
def factorial(n):
    return 1 if n == 0 else factorial(n-1)*n


def t_sum(permu_count):
    """
    Sum of T(n) for all n in the permutation group
    """
    if permu_count == 1:
        return 0
    return nCr(permu_count, 2)


def digits_combo_count(k, unique_digits):
    """
    The number of way to to form a k-digit number with all u number of unique digits

    Equivalent to the number of u-tuple of positive integers, whose sum is k
    https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)
    """
    if k < unique_digits:
        return 0
    return nCr(k-1, unique_digits-1)


@cache
def digits_combo(k, unique_digits):
    """
    Combinations to form a k-digit number with u number of unique digits

    Returns a list of frequency of each digit.
    """
    if k <= 0 or k < unique_digits:
        return []
    if unique_digits == 1:
        return [[k]]

    res = []
    for i in range(1, k):
        remain = digits_combo(k-i, unique_digits-1)
        for r in remain:
            res.append([i] + r)

    assert len(res) == digits_combo_count(k, unique_digits)

    return res


def permutations(k, combo):
    """
    Precondition: k == sum(combo)

    Does not handle leading 0
    """
    permu_count = factorial(k)
    for c in combo:
        permu_count //= factorial(c)
    return permu_count


def without0_permu(k, unique_digits):
    permu_counts = []
    for combo in digits_combo(k, unique_digits):
        permu_counts.append(permutations(k, combo))
    return permu_counts


def with0_permu(k, unique_digits):
    """
    0 used at least once;
    unique_digits includes 0, which means 2 <= unique_digits <= 10
    """
    permu_counts = []
    for combo in digits_combo(k, unique_digits):
        permu_count = 0
        for i in range(1, len(combo)):
            combo[i] -= 1
            # pick a non-0 digit as the leading digit
            permu_count += permutations(k-1, combo)
            combo[i] += 1
        permu_counts.append(permu_count)

    return permu_counts


def S(K):
    ###
    ### without 0
    ###
    print([
        (digits_combo_count(K, i), nCr(9, i))
        for i in range(1, 10)
    ])
    assert sum(
        sum(without0_permu(K, i)) * nCr(9, i)
        for i in range(1, 10)
    ) == 9**K

    print([
        (sum(t_sum(p) for p in without0_permu(K, i)), nCr(9, i))
        for i in range(1, 10)
    ])
    S_without0 = sum(
        sum(t_sum(p) for p in without0_permu(K, i)) * nCr(9, i)
        for i in range(1, 10)
    )
    print('S_without0', S_without0)

    ###
    ### with 0
    ###
    print([
        (digits_combo_count(K, i), nCr(9, i-1))
        for i in range(2, 11)
    ])
    assert sum(
        sum(with0_permu(K, i)) * nCr(9, i-1)
        for i in range(2, 11)
    ) == 9*10**(K-1) - 9**K

    print([
        (sum(t_sum(p) for p in with0_permu(K, i)), nCr(9, i-1))
        for i in range(2, 11)
    ])
    S_with0 = sum(
        sum(t_sum(p) for p in with0_permu(K, i)) * nCr(9, i-1)
        for i in range(2, 11)
    )
    print('S_with0', S_with0)

    return S_without0 + S_with0


if __name__ == '__main__':
    print(S(12))
