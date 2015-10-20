import numpy as np


def main():
    N = 40
    cache = np.zeros((10, N, 1024), dtype=np.int64)
    mask = np.zeros((10, N, 1024), dtype=np.bool)

    def get_count(curr_digit, digits_left, contains):
        if digits_left == 0:
            if contains == 1023:
                return 1
            else:
                return 0

        if mask[curr_digit, digits_left, contains]:
            pass
        elif curr_digit == 0:  # choose digit 1
            cache[curr_digit, digits_left, contains] = get_count(1, digits_left-1, contains | 0b10)
            mask[curr_digit, digits_left, contains] = True
        elif curr_digit == 9:  # choose digit 8
            cache[curr_digit, digits_left, contains] = get_count(8, digits_left-1, contains | 2**8)
            mask[curr_digit, digits_left, contains] = True
        else:  # choose curr_digit-1, curr_digit+1
            cache[curr_digit, digits_left, contains] = get_count(curr_digit-1, digits_left-1, contains | 2**(curr_digit-1)) + \
                                                   get_count(curr_digit+1, digits_left-1, contains | 2**(curr_digit+1))
            mask[curr_digit, digits_left, contains] = True
        return cache[curr_digit, digits_left, contains]

    total = sum(sum(get_count(i, digit, 2**i) for i in range(1, 10)) for digit in range(1, N))
    print(total)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
