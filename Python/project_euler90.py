from itertools import combinations


def either(digits, cube1, cube2):
    if not (6 in digits or 9 in digits):
        return (digits[0] in cube1 and digits[1] in cube2) or \
               (digits[0] in cube2 and digits[1] in cube1)
    if digits[0] == 6 or digits[0] == 9:
        return ((6 in cube1 or 9 in cube1) and digits[1] in cube2) or \
               ((6 in cube2 or 9 in cube2) and digits[1] in cube1)
    if digits[1] == 6 or digits[1] == 9:
        return ((6 in cube1 or 9 in cube1) and digits[0] in cube2) or \
               ((6 in cube2 or 9 in cube2) and digits[0] in cube1)


def check(cube1, cube2):
    return all(either(i, cube1, cube2) for i in squares)

squares = [(0, 1), (0, 4), (0, 9), (1, 6), (2, 5), 
           (3, 6), (4, 9), (6, 4), (8, 1)]


def main():
    combs = list(combinations(range(10), 6))
    count = 0
    for i in range(len(combs)):
        for j in range(i+1, len(combs)):
            if check(combs[i], combs[j]):
                count += 1
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
