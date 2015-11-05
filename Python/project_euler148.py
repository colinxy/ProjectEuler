from math import log, floor

"""
def to_base(n, base, alphabet="0123456789abcdefghijklmnopqrstuvwxyz"):
    rep = ''
    while n > 0:
        n, digit = divmod(n, base)
        rep = alphabet[digit] + rep
    return rep


def sum_1_n(n):
    return n * (n + 1) // 2


def sum_n(num, n):
    return (n + (n - num + 1)) * num // 2


def pascal_7(floors):
    total_blocks = floors * (floors + 1) // 2
    return 21 * total_blocks


def pascal(rows):
    if rows < 7:
        return

    layers = floor(log(rows, 7))
    extra = rows - 7 ** layers

    floors = rows // 7
    multiple_of_7 = pascal_7(floors)
    l = (7 - rows % 7)
    cut_off = floors * l * (l - 1) // 2
    return multiple_of_7 - cut_off
"""


def main():
    """
    layers = [0]
    for i in range(1, 11):
        this_layer = sum_1_n(7) * layers[i-1] + sum_1_n(6) * sum_1_n(7**i-1)
        layers.append(this_layer)
    print(layers)

    base7 = to_base(10**9, 7)
    print(base7)  # 33531600616
    base7 = list(map(int, base7))

    pascal7 = 0

    for i in reversed(range(len(base7))):
        pascal7 += layers[i-1] * sum_1_n(base7[10-i]) + sum_1_n(7**10-1) * sum_1_n(base7[10-i]-1)
        # for j in range()

    # 3 * 7 ** 10
    pascal7 += layers[9] * sum_1_n(3) + sum_1_n(7**10-1) * sum_1_n(2)

    # 3 * 7 ** 9
    pascal7 += sum_n(3*7**9-1, 7**10-1) * 3
    pascal7 += (layers[8] * sum_1_n(3) + sum_1_n(7**9-1) * sum_1_n(2)) * 4

    # 3 * 7 ** 8
    # pascal7 += sum_1_n()


    total = sum_1_n(10 ** 9)

    rows = 7*7  # 10**9
    total = rows * (rows + 1) // 2
    pascal7 = pascal(rows)
    print(rows)
    print(total)
    print("divisible by 7:", pascal7, sep='\n')
    print("not divisible by 7:", total - pascal7, sep='\n')
    """

    # brute force
    print('\n')

    def next_pascal(row):
        return [row[0]] + [(row[_ - 1] + row[_]) % 7 for _ in range(1, len(row))] + [row[-1]]

    N = 10 ** 4
    count = 28
    current = [1, 6, 15, 20, 15, 6, 1]
    for i in range(8, N + 1):
        current = next_pascal(current)
        count += sum(j != 0 for j in current)

    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time() - starting_time, "seconds")
