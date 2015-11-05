import numpy as np


def main():
    matrix = np.empty((15, 15), dtype=np.int32)
    occupied = np.zeros((15, 15), dtype=np.bool)
    matrix_sum = []

    with open("p345_matrix.txt") as f:
        for index, line in enumerate(f):
            matrix[index] = list(map(int, line.split()))
    # print(matrix)

    # greedy search for a suboptimal solution
    for i in reversed(range(15)):
        line_max = 0
        index = 0
        for j in range(15):
            if not occupied[i, j] and matrix[i, j] > line_max:
                line_max = matrix[i, j]
                index = j
        occupied[i].fill(1)
        occupied[:, index].fill(1)
        matrix_sum.append((i, index))
    # print(matrix_sum)

    # gradually improve matrix sum
    changed = True
    while changed:
        changed = False
        for i in range(15):
            for j in range(i + 1, 15):
                p1_x, p1_y = matrix_sum[i]
                p2_x, p2_y = matrix_sum[j]
                if matrix[p1_x, p2_y] + matrix[p2_x, p1_y] > matrix[p1_x, p1_y] + matrix[p2_x, p2_y]:
                    matrix_sum[i], matrix_sum[j] = (p1_x, p2_y), (p2_x, p1_y)
                    changed = True
                    print(sum(matrix[pos] for pos in matrix_sum))
    print(matrix_sum)
    print(sum(matrix[pos] for pos in matrix_sum))


# todo: rewrite it as a deterministic algorithm
# algorithm is heuristic
if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
