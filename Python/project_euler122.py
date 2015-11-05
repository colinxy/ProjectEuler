# based on optimal substructure
def branching(current):
    num = len(current)
    for i in reversed(current):
        target = current[-1] + i
        if target > N:
            continue

        if num <= path_len[target]:
            if path_len[target] == num:
                path_len[target] = num
                # solution[target].append(current + [target])
            else:
                path_len[target] = num
                # solution[target] = [current + [target]]
            branching(current + [target])


N = 200
path_len = [1000000] * (N+1)  # inf
path_len[0], path_len[1], path_len[2] = 0, 0, 1
# solution = [[] for _ in range(N+1)]


def main():
    current = [1, 2]
    branching(current)

    print(list(enumerate(path_len)))
    print(sum(path_len))
    # print(solution)

    # code credit to Talha Zaman on google code
    # although I was writing the correct code, I was somehow convinced that my code was too slow,
    # with a optimization inspired by the following code, my code was also blazingly fast
    # path = [[list(range(1, i+1))] for i in range(N+1)]
    # for i in range(1, len(path)):
    #     for j in path[i]:
    #         for k in [a for a in j if i+a<len(path)]:
    #             if len(path[i][0])+1 < len(path[i+k][0]):
    #                 path[i+k] = [j + [i+k]]
    #             elif len(path[i][0])+1 == len(path[i+k][0]):  # and len(path[i+k]) < 5:
    #                 path[i+k].append(j + [i+k])
    # print(sum(len(p[0])-1 for p in path[1:]))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
