import numpy as np


HORIZONTAL = 32
VERTICAL = 10


def is_crack_free(combin1, combin2):
    sum1 = combin1[0]
    sum2 = combin2[0]
    i1, i2 = 1, 1
    while True:
        if sum1 == sum2 == HORIZONTAL:
            return True
        if sum1 == sum2:
            return False

        if sum1 < sum2:
            sum1 += combin1[i1]
            i1 += 1
        else:
            sum2 += combin2[i2]
            i2 += 1


def main():
    blocks = [[2], [3]]
    combinations = []
    while blocks:
        current = blocks.pop()
        current_sum = sum(current)

        if current_sum == HORIZONTAL:
            combinations.append(current)
        elif current_sum > HORIZONTAL:
            pass
        else:
            blocks.append(current + [2])
            blocks.append(current + [3])

    num_of_combin = len(combinations)
    # print(num_of_combin)
    crack_free_cache = [[] for _ in range(num_of_combin)]

    for i1 in range(num_of_combin):
        for i2 in range(i1+1, num_of_combin):
            if is_crack_free(combinations[i1], combinations[i2]):
                crack_free_cache[i1].append(i2)
                crack_free_cache[i2].append(i1)

    # print("crack free cache generated")
    cache = np.zeros((num_of_combin, VERTICAL), dtype=np.int64)

    for i in range(num_of_combin):
        cache[i, 1] = len(crack_free_cache[i])

    def fill_walls(curr_combin, left_vertical):
        if cache[curr_combin, left_vertical] > 0 or left_vertical == 1:
            return cache[curr_combin, left_vertical]

        cache[curr_combin, left_vertical] = \
            sum(fill_walls(comb, left_vertical-1)
                for comb in crack_free_cache[curr_combin])
        return cache[curr_combin, left_vertical]

    print(sum(fill_walls(comb, VERTICAL-1) for comb in range(num_of_combin)))


if __name__ == '__main__':
    main()
