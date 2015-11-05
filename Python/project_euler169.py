def group_solve(groups_in_int):
    global cache0, cache1
    cache0, cache1 = [0] * (len(groups_in_int) + 1), [0] * (len(groups_in_int) + 1)
    return solve(groups_in_int)


def solve(groups_in_int):
    cache_index = len(groups_in_int)
    if cache_index == 1:
        return groups_in_int[0]
    if cache0[cache_index] != 0:
        return cache0[cache_index]
    cache0[cache_index] = groups_in_int[0] * solve(groups_in_int[1:]) + _solve([groups_in_int[1]-1] + groups_in_int[2:])
    return cache0[cache_index]


# for the altered input
def _solve(groups_in_int):
    cache_index = len(groups_in_int)
    if cache_index == 1:
        return groups_in_int[0]
    if cache1[cache_index] != 0:
        return cache1[cache_index]
    cache1[cache_index] = groups_in_int[0] * solve(groups_in_int[1:]) + _solve([groups_in_int[1]-1] + groups_in_int[2:])
    return cache1[cache_index]

cache0 = []
cache1 = []


def sum_power(N):
    bin_rep = bin(N)
    # print(bin_rep)
    # print(bin_rep.split('1'))

    # groups = []
    # i = 2
    # bin_len = len(bin_rep)
    # while i < bin_len:
    #     if bin_rep[i] == '1':
    #         length = 1
    #         i += 1
    #         while i < len(bin_rep) and bin_rep[i] == '0':
    #             length += 1
    #             i += 1
    #         groups.append(bin_rep[i-length:i])
    # groups_in_int = list(map(len, groups))
    # print(groups_in_int)

    groups_in_int = list(map(lambda s: len(s) + 1, bin_rep.split('1')))[1:]
    # print(groups_in_int)

    global cache0, cache1
    cache0, cache1 = [0] * (len(groups_in_int) + 1), [0] * (len(groups_in_int) + 1)

    return solve(groups_in_int)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    print(sum_power(10 ** 25))
    print("Time elapsed:", time() - starting_time, "seconds")
