from math import ceil, sqrt


def main():
    side = 1000
    tiles = side * side  # number of tiles has to be a perfect square, in this case

    print("Fast method")
    t = tiles // 4
    l = int(sqrt(t)) + 1
    print("solution:", sum(t//i-i for i in range(1, l)))
    print("Time elapsed:", time() - starting_time, "seconds")

    result = side // 2 * (side // 2 - 1)  # (1 + 499) * 499
    for i in range(side+1, tiles//2+2):
        end = ceil(sqrt(i * i - tiles))
        result += (i - end) // 2

    print(result)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
