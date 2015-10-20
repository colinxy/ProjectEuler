from mathutil import nCr


def main():
    size = 250250
    div = 250
    mod = 10 ** 16

    remainder = [0] * div
    cache = [[0] * div for _ in range(div)]
    # cache = np.zeros((div, div), dtype=np.int64)

    for i in range(1, size + 1):
        remainder[0 if i % 10 == 0 else pow(i, i, 250)] += 1
    # print(remainder)

    cache[0][0] = pow(2, remainder[0], mod) - 1
    for rem in range(1, div):
        print(rem)

        change = [0] * div
        for c in range(remainder[rem]+1):
            change[rem * c % div] = (change[rem * c % div] + nCr(remainder[rem], c)) % mod
        # print(change)

        for index, i in enumerate(change):
            for j in range(div):
                new = cache [rem] [(index+j)%div] + i * cache [rem-1] [j]
                cache [rem] [(index+j)%div] = new % mod
        # print(rem, cache[rem])

    print(cache[-1][0] - 1)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    # main()
    print("Time elapsed:", time() - starting_time, "seconds")
