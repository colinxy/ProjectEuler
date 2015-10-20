import numpy as np
from mathutil import nCr


def main():
    """
    def get_digits(n):
        return list(map(int, str(n)))
    same = 0
    increasing = 0
    decreasing = 0
    for i in range(1, 10 ** 6):
        all_digits = get_digits(i)

        if all([j == all_digits[0] for j in all_digits]):
            same += 1
            continue

        all_increasing = all(all_digits[j-1] <= all_digits[j] for j in range(1, len(all_digits)))
        if all_increasing:
            increasing += 1
            continue

        all_decreasing = all(all_digits[j-1] >= all_digits[j] for j in range(1, len(all_digits)))
        if all_decreasing:
            decreasing += 1

    count = same + increasing + decreasing
    print(same, increasing, decreasing)
    print(count, 10**6-1 - count)
    # experiments finished
    """

    digits = 100

    increase_cache = np.zeros((10, digits+1), dtype=np.int64)  # 1~9  increase_cache[a, b] --> all digits <= a; b digits
    increase_cache[1:, 0] = [1] * 9
    # print(increase_cache)
    for i in range(1, digits+1):
        for j in range(1, 10):
            increase_cache[j, i] = increase_cache[j-1, i] + increase_cache[j, i-1]
    increase = sum(increase_cache[9, i] for i in range(1, digits+1))
    # print(increase_cache)
    print(increase)

    decrease_cache = np.zeros((11, digits+1), dtype=np.int64)  # 9~0  decrease_cache[a, b] --> all digits >= a; b digits
    decrease_cache[1:10, 0] = [1] * 9
    # print(decrease_cache)
    for i in range(1, digits+1):
        for j in range(9, -1, -1):
            decrease_cache[j, i] = decrease_cache[j+1, i] + decrease_cache[j, i-1]
    decrease = sum(decrease_cache[0, i] for i in range(1, digits+1))
    # print(decrease_cache)
    print(decrease)

    result = increase + decrease - 9 * digits  # 11111111...  ~  99999999...
    print(result)

    print("\nAnother extremely elegant solution:")
    count = 0
    for i in range(1, 101):
        count += nCr(i+8, i)  # increase up to 8 times
        count += nCr(i+9, i)  # decrease up to 9 times
        count -= 10
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
