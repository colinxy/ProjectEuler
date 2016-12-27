# Problem 287: Quadtree encoding (a simple compression algorithm)

from __future__ import print_function, division

N = 24
SIDE = 2**N


def is_black(x, y):
    # within circle
    return (x - SIDE//2)**2 + (y - SIDE//2)**2 <= (SIDE//2)**2


def min_sequence(side_len, top_left_x, top_left_y):
    """NOTE: cannot be invoked on the entire image"""
    if side_len == 1:
        return 2

    top_right_x = top_left_x + side_len - 1
    top_right_y = top_left_y

    bottom_left_x = top_left_x
    bottom_left_y = top_left_y - side_len + 1

    bottom_right_x = top_left_x + side_len - 1
    bottom_right_y = top_left_y - side_len + 1

    black = [is_black(x, y) for x, y in ((top_left_x, top_left_y),
                                         (top_right_x, top_right_y),
                                         (bottom_left_x, bottom_left_y),
                                         (bottom_right_x, bottom_right_y))]
    if all(black) or all(map(lambda b: not b, black)):
        return 2

    # if not is_black(bottom_right_x, bottom_right_y):
    #     # completely white
    #     return 2
    # # bottom_right black
    # if is_black(top_left_x, top_left_y):
    #     # completely black
    #     return 2
    # # top_left white

    return 1 + (
        # top_left in subregion
        min_sequence(side_len//2,
                     top_left_x, top_left_y) +
        # top_right in subregion
        min_sequence(side_len//2,
                     top_left_x+side_len//2, top_left_y) +
        # bottom_left in subregion
        min_sequence(side_len//2,
                     top_left_x, top_left_y-side_len//2) +
        # bottom_right in subregion
        min_sequence(side_len//2,
                     top_left_x+side_len//2, top_left_y-side_len//2)
    )


def min_sequence_len():
    # assume N > 2, and there is always an initial split
    top_left = min_sequence(SIDE//2, 0, SIDE-1)
    top_right = min_sequence(SIDE//2, SIDE//2, SIDE-1)
    bottom_left = min_sequence(SIDE//2, 0, SIDE//2-1)
    bottom_right = min_sequence(SIDE//2, SIDE//2, SIDE//2-1)
    return 1 + top_left + top_right + bottom_left + bottom_right


def main():
    # for y in range(2**N-1, -1, -1):
    #     for x in range(2**N):
    #         print('X' if is_black(y, x) else '-', end='')
    #     print()

    # print("python is way too slow, run with pypy (around 30s)")
    print(min_sequence_len())


if __name__ == '__main__':
    main()
