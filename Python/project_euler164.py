import numpy as np


def main():
    """
    import itertools
    print([10, 9, 8, 7, 6, 5, 4, 3, 2, 1], '\n')

    solution_count = [0] * 10
    for i in itertools.product(range(10), repeat=3):
        three_consecutive = True
        for index in range(len(i)-2):
            if sum(i[index:index+3]) > 9:
                three_consecutive = False
                break
        if three_consecutive:
            solution_count[i[-1]] += 1

    print(solution_count, sum(solution_count))
    print([sum(solution_count[:i+1]) for i in range(len(solution_count))])
    print([sum(solution_count[i:]) for i in range(len(solution_count))], '\n')

    solution_count = [0] * 10
    for i in itertools.product(range(10), repeat=4):
        three_consecutive = True
        for index in range(len(i)-2):
            if sum(i[index:index+3]) > 9:
                three_consecutive = False
                break
        if three_consecutive:
            solution_count[i[-1]] += 1

    print(solution_count, sum(solution_count), '\n')

    solution_count = [0] * 10
    for i in itertools.product(range(10), repeat=5):
        three_consecutive = True
        for index in range(len(i)-2):
            if sum(i[index:index+3]) > 9:
                three_consecutive = False
                break
        if three_consecutive:
            solution_count[i[-1]] += 1

    print(solution_count, sum(solution_count), '\n')

    solution_count = [0] * 10
    for i in itertools.product(range(10), repeat=6):
        three_consecutive = True
        for index in range(len(i)-2):
            if sum(i[index:index+3]) > 9:
                three_consecutive = False
                break
        if three_consecutive:
            solution_count[i[-1]] += 1

    print(solution_count, sum(solution_count), '\n')
    """

    cache = np.zeros((10, 10, 21), dtype=np.int64)

    def get_count(d2, d3, remain_digits):     # d1, d2, d3;  d1+d2+d3 < 9
        if remain_digits == 0:
            return 1
        else:
            if cache[d2, d3, remain_digits] != 0:
                return cache[d2, d3, remain_digits]
            else:
                if remain_digits > 1:
                    cache[d2, d3, remain_digits] = sum(get_count(i, d2, remain_digits-1) for i in range(9-d2-d3+1))
                else:
                    cache[d2, d3, remain_digits] = sum(get_count(i, d2, remain_digits-1) for i in range(1, 9-d2-d3+1))
                return cache[d2, d3, remain_digits]

    print(get_count(0, 0, 20))
    # np.set_printoptions(linewidth=1000); print(cache[0, :, :])


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("\nTime elapsed:", time() - starting_time, "seconds")
