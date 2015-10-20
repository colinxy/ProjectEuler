import numpy as np


# maintain length 10
def to_base4(index):
    rep = ''
    for i in range(10):
        index, digit = divmod(index, 4)
        rep = str(digit) + rep
    return rep


# turn digit_dict to index
# {0: 3, 1: 3, 2: 3, 3: 3, 4: 3, 5: 3, 6: 3, 7: 3, 8: 3, 9: 3} --> index largest
# '3333333333' --> index
# use list instead
def hash_func(digits):
    base4 = ''.join(str(digits[i]) for i in range(10))
    return int(base4, 4)


def hash_func_rev(index):
    list_in_str = to_base4(index)
    return [int(list_in_str[i]) for i in range(10)]


def use_digit(index, number):
    return index - 4 ** (9 - number)


def main():
    cache = np.zeros((4 ** 10, 19), dtype=np.int64)
    cache[:, 0] = [1] * 4 ** 10

    def get_count(index, size):
        if cache[index, size] != 0:
            return cache[index, size]
        else:
            digit_list = hash_func_rev(index)
            count = 0
            if size == 1:
                for i in range(1, 10):
                    if digit_list[i] > 0:
                        # tmp_digit_list = digit_list.copy()
                        # tmp_digit_list[i] -= 1
                        count += get_count(use_digit(index, i), size-1)
            else:
                for i in range(10):
                    if digit_list[i] > 0:
                        # tmp_digit_list = digit_list.copy()
                        # tmp_digit_list[i] -= 1
                        count += get_count(use_digit(index, i), size-1)
            cache[index, size] = count
            return count

    print(get_count(4**10-1, 18))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
