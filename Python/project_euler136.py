# (a+d)**2 - a**2 - (a-d)**2 = 4*a*d - a*a
#                            = a * (4*d - a)
#                              a > d, 4*d > a

from math import sqrt, ceil


def solve_quad(a, b, c):
    """
    solve quadratic function of the form ax^2 + bx + c = 0
    """
    delta = b**2 - 4*a*c
    if delta < 0:
        raise ValueError("delta < 0")
    return (-b - sqrt(delta)) / (2*a), (-b + sqrt(delta)) / (2*a)


def main():
    n = 50000000

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
    unique = [(i, j) for i, j in enumerate(solutions) if j == 1]
    print(len(unique))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
