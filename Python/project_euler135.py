from math import sqrt, ceil


def solve_quad(a, b, c):
    """
    solve quadratic function of the form ax^2 + bx + c = 0
    """
    delta = b**2 - 4*a*c
    if delta < 0:
        raise ValueError("delta < 0")
    return (-b - sqrt(delta)) / (2*a), (-b + sqrt(delta)) / (2*a)


def num_of_solution(n):
    count = 0
    for i in range(1, n+1):
        if n % i == 0:
            d4 = n // i + i
            if d4 % 4 == 0 and i > d4 // 4:
                count += 1
    return count


def main():
    n = 1000000

    solutions = [0] * (n+1)

    divide = int(sqrt(n) / 2) + 1
    for d in range(1, divide):
        for a in range(d+1, 4*d):
            solutions[a*(4*d-a)] += 1

    for d in range(divide, n+1):
        s1, s2 = solve_quad(1, -4*d, n)
        for a in range(d+1, int(s1)+1):
            solutions[a*(4*d-a)] += 1
        for a in range(ceil(s2), 4*d):
            solutions[a*(4*d-a)] += 1

    # print(sum(solutions))
    # print(max(enumerate(solutions), key=lambda l: l[1]))
    ten_distinct = [(i, j) for i, j in enumerate(solutions) if j == 10]
    print(len(ten_distinct))

    # for index, value in enumerate(solutions):
    #     print(index, value)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
