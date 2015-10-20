from itertools import product


def is_special(the_set):  # must be presorted
    sums = [False] * (sum(the_set) + 1)
    length = len(the_set)

    for i in range(1, len(the_set)):
        if sum(the_set[length-i:]) >= sum(the_set[:i+1]):
            return False

    for mask in product(range(2), repeat=len(the_set)):
        the_sum = sum(j * k for j, k in zip(the_set, mask))
        if sums[the_sum]:
            return False
        sums[the_sum] = True
    return True


def main():
    # print(is_special(sorted([157, 150, 164, 119, 79, 159, 161, 139, 158])))
    # print(is_special(sorted([81, 88, 75, 42, 87, 84, 86, 65])))
    result = 0
    with open("p105_sets.txt", 'r') as f:
        for line in f:
            the_set = sorted(map(int, line.split(',')))
            if is_special(the_set):
                result += sum(the_set)
    print(result)


if __name__ == '__main__':
    from time import time

    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
