# Problem 158: Exploring strings for which only one character
# comes lexicographically after its neighbour to the left

# \
#  \
#   \  | \
#    \ |  \
#          \
#           \

from mathutil import nCr


def lexi_after_total(alpha):
    total = 0
    for i in range(alpha):
        for j in range(i+1, alpha):
            total += 2**i * 3**(j-i-1) * 2**(alpha-j-1)
    return total


def pick_n(arr, n):
    if len(arr) == 0 or n < 0:
        return []
    if len(arr) == 1:
        return [(n,)] if n <= arr[0] else []

    res = [(0,) + p for p in pick_n(arr[1:], n)]
    for i in range(1, min(arr[0], n)+1):
        res.extend([(i,) + p for p in pick_n(arr[1:], n-i)])

    return res


def lexi_after(alpha, n):
    count = 0
    for i in range(alpha):
        for j in range(i+1, alpha):
            for a, b, c in pick_n((i, j-i-1, alpha-j-1), n-2):
                count += nCr(i, a) * nCr(j-i-1, b) * 2**b * nCr(alpha-j-1, c)
    return count


def main():
    N = 26
    # print(lexi_after_total(N))
    # print(sum(lexi_after(N, i) for i in range(1, N+1)))

    print(max(lexi_after(N, i) for i in range(1, N+1)))

    # SMQ's brilliant explanation
    # p(n) = nCr(26, n) * sum((nCr(n, k) - 1) for k in range(1, n))
    # or equivalently
    # p(n) = nCr(26, n) * (2**n - n - 1)


if __name__ == '__main__':
    main()
