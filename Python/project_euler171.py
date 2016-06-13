
def is_square(x):
    return int(x ** 0.5) ** 2 == x


def main():
    DIGITS = 20

    targets_dict = [{} for _ in range(DIGITS)]
    targets_dict[0] = {i**2: [(i, 0)] for i in range(10)}
    for d in range(1, DIGITS):
        current = targets_dict[d]

        for i in range(10):
            for old_target in targets_dict[d-1]:
                new_target = old_target + i**2
                current.setdefault(new_target, []).append((i, old_target))

    # for i in range(5):
    #     for j in sorted(targets_dict[i]):
    #         print(j, targets_dict[i][j], end='  ')
    #     print()

    times_used_cache = [{} for _ in range(DIGITS)]
    times_used_cache[0] = {i**2: [1] for i in range(10)}

    def get_times_used_count(target, digits_left):
        if times_used_cache[digits_left].get(target, []):
            return sum(times_used_cache[digits_left][target])

        target_counts = []
        for curr, last_target in targets_dict[digits_left].get(target, []):
            curr_target_count = get_times_used_count(last_target,
                                                     digits_left-1)
            target_counts.append(curr_target_count)
        times_used_cache[digits_left][target] = target_counts

        return sum(target_counts)

    for t in targets_dict[DIGITS-1]:
        get_times_used_count(t, DIGITS-2)

    # for i in range(5):
    #     for j in sorted(times_used_cache[i]):
    #         print(j, times_used_cache[i][j], end='  ')
    #     print()

    sum_cache = [{} for _ in range(DIGITS)]
    sum_cache[0] = {i**2: [i] for i in range(10)}

    def get_sum(target, digits_left):
        if sum_cache[digits_left].get(target, 0) or digits_left == 0:
            return sum(sum_cache[digits_left][target])

        target_sums = []
        for curr, last_target in targets_dict[digits_left].get(target, []):
            last_target_sum = get_sum(last_target, digits_left-1)
            # print(curr, last_target, times_used_cache[digits_left][last_target])
            curr_digit_sum = last_target_sum + \
                curr * 10**digits_left * \
                sum(times_used_cache[digits_left-1][last_target])
            target_sums.append(curr_digit_sum)
        sum_cache[digits_left][target] = target_sums

        return sum(target_sums)

    for t in targets_dict[DIGITS-1]:
        get_sum(t, DIGITS-1)

    # for i in range(5):
    #     for j in sorted(sum_cache[i]):
    #         print(j, sum_cache[i][j], end='  ')
    #     print()

    total = 0
    for d in range(DIGITS):
        for t in targets_dict[d]:
            if is_square(t):
                for (curr_digit, old_target), target_sum in \
                        zip(targets_dict[d][t], sum_cache[d][t]):
                    if curr_digit != 0:
                        total += target_sum

    print(total)
    print(str(total)[-9:])


if __name__ == '__main__':
    main()
