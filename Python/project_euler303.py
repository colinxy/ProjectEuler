from mathutil import to_base


def f_n_div_n(n):
    i = 1
    num = n
    while True:
        if all('0' <= s <= '2' for s in str(num)):
            return i
        i += 1
        num += n


def knapsack2(n, size, items):
    if 2 * sum(items) < size:
        return False

    if size == 0:
        return [0] * len(items)

    if len(items) == 1:
        if size % items[0] == 0 and size // items[0] <= 2:
            return [size // items[0]]
        return False

    pick0 = knapsack2(n, size, items[1:])
    if isinstance(pick0, list):
        return [0] + pick0

    pick1 = knapsack2(n, (size-items[0]) % n, items[1:])
    if isinstance(pick1, list):
        return [1] + pick1

    pick2 = knapsack2(n, (size-2*items[0]) % n, items[1:])
    if isinstance(pick2, list):
        return [2] + pick2
    return False


def f_n_given_digit(n, digits=0):
    while True:
        remainders = [pow(10, i, n) for i in range(digits, -1, -1)]

        result = knapsack2(n, (n - remainders[0]) % n, remainders[1:])
        if isinstance(result, list):
            return int('1' + ''.join(map(str, result)))

        result = knapsack2(n, (n - 2*remainders[0]) % n, remainders[1:])
        if isinstance(result, list):
            return int('2' + ''.join(map(str, result)))

        digits += 1


def main():
    N = 10000
    cutoff_estimate = 12
    results = [0] * (N+1)

    # solution1
    print(sum(f_n_given_digit(i) // i for i in range(1, N+1)))
    print("solution1:", time() - starting_time, "seconds\n")

    # another solution, split into 2 parts, but slower
    left = set(range(1, N+1))
    for i in range(1, 3**cutoff_estimate):
        num = int(to_base(i, 3))
        peel = set()
        for fact in left:
            if num % fact == 0:
                results[fact] = num
                peel.add(fact)
                # print(fact, num)
        left -= peel

    left = sorted(left)
    # print(left)
    # print("Time elapsed:", time() - starting_time, "seconds")

    for n in left:
        results[n] = f_n_given_digit(n, cutoff_estimate)

    # print(results)
    print(sum(results[i] // i for i in range(1, N+1)))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
