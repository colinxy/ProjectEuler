# Harshad Numbers

from collections import deque
from mathutil import is_prime


def digit_sum(num):
    return sum(map(int, str(num)))


def next_harshads(harshad_tuple):
    num_orig = harshad_tuple[0]

    harshads = []
    for d in range(10):
        num = num_orig * 10 + d
        div, mod = divmod(num, digit_sum(num))
        if mod == 0:
            harshads.append((num, is_prime(div)))

    return harshads


def harshad_prime(num):
    return [num * 10 + d for d in [1, 3, 7, 9] if is_prime(num * 10 + d)]

SIZE = 10 ** 14


def main():
    # tuple repr (number: int, has_prime: bool)
    choice = deque((num, False) for num in range(1, 10))
    strong_rtrunc = []

    # count = 0
    while choice[0][0] < SIZE / 10:
        # count += 1
        tup = choice.popleft()
        if tup[1]:
            strong_rtrunc.append(tup[0])
        choice.extend(next_harshads(tup))

    # print(count)
    # for num in strong_rtrunc:
    #     print(harshad_prime(num)
    print(sum(sum(harshad_prime(num)) for num in strong_rtrunc))


if __name__ == '__main__':
    main()
