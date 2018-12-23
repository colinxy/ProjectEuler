# Problem 511: Sequences with nice divisibility properties

from collections import defaultdict

from mathutil import get_factors

MOD = 10 ** 9


def seq(n, k):
    factors = get_factors(n)
    remainder_map = defaultdict(int)
    for f in factors:
        remainder_map[f % k] += 1

    def subseq(length):
        if length == 1:
            return remainder_map
        sub_left = subseq(length // 2)
        sub_right = sub_left
        if length % 2 == 1:
            sub_right = merge(sub_right, remainder_map)
        return merge(sub_left, sub_right)

    def merge(sub_left, sub_right):
        """convolution
        https://en.wikipedia.org/wiki/Convolution#Fast_convolution_algorithms

        This implementation here runs in O(N^2)

        This algorithm can be improved to run in O(NlogN) time, but
        with imprecision using fft. Refer to Yoni's post
        https://projecteuler.net/thread=511#311921 for details for a
        brilliant solution.
        """
        res = defaultdict(int)
        for l_rem, l_count in sub_left.items():
            for r_rem, r_count in sub_right.items():
                res[(l_rem+r_rem) % k] = (
                    res[(l_rem+r_rem) % k] + l_count*r_count
                ) % MOD
        return res

    return subseq(n)[k - n % k]


def main():
    print(seq(3, 4))
    print(seq(4, 11))
    print(seq(1111, 24))
    print(seq(1234567898765, 4321))  # 40s with pypy


if __name__ == '__main__':
    main()
