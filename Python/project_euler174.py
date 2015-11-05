from math import sqrt, ceil


def count(p):
    result = 0
    half_p = p // 2
    for i in range(2, ceil(sqrt(p)), 2):
        if half_p % i == 0:
            result += 1
    return result


def main():
    each = [0] * 1000001
    for i in range(8, 1000001, 4):
        n = count(i)
        each[i] = n
    print(sum(1 <= i <= 10 for i in each))
    print(sum(each))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
