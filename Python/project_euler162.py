import numpy as np


def main():
    cache = np.zeros((8, 17), dtype=np.int64)
    cache[:, 1] = [13, 0, 1, 1, 0, 0, 0, 0]

    status_map = {'': 0, '0': 1, '1': 2, 'A': 3, '01': 4, '0A': 5, '1A': 6, '01A': 7}

    def get_count(status, remain_digits):
        if remain_digits == 1:
            return cache[status, remain_digits]
        else:
            if cache[status, remain_digits] != 0:
                return cache[status, remain_digits]
            else:
                if status == 0:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 13
                elif status == 1:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 14 + sum(get_count(i, remain_digits-1) for i in [0])  # '': 0
                elif status == 2:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 14 + sum(get_count(i, remain_digits-1) for i in [0])  # '': 0
                elif status == 3:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 14 + sum(get_count(i, remain_digits-1) for i in [0])  # '': 0
                elif status == 4:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 15 + sum(get_count(i, remain_digits-1) for i in [1, 2])  # '0': 1, '1': 2
                elif status == 5:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 15 + sum(get_count(i, remain_digits-1) for i in [1, 3])  # '0': 1, 'A': 3
                elif status == 6:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 15 + sum(get_count(i, remain_digits-1) for i in [2, 3])  # '1': 2, 'A': 3
                elif status == 7:
                    cache[status, remain_digits] = get_count(status, remain_digits-1) * 16 + sum(get_count(i, remain_digits-1) for i in [4, 5, 6])  # '01': 4, '0A': 5, '1A': 6

                return cache[status, remain_digits]

    digits = 16
    result = sum(get_count(7, i) for i in range(1, digits+1))
    # print(cache)
    print(result)
    print(hex(result).upper())


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
