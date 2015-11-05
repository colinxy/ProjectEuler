from itertools import combinations
from mathutil import nCr


def check(k):
    count = 0
    for j in combinations(range(1, 2*k), k):
        other = set(range(1, 2*k+1)) - set(j)
        for k, l in zip(j, sorted(other)):
            if k >= l:
                count += 1
                break
    return count


def main():
    n = 12

    count = 0
    for i in range(2, n//2+1):
        this_count = check(i)

        this_count *= nCr(n, 2*i)
        count += this_count

    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
