# Cuboid route
"""
A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3,
and a fly, F, sits in the opposite corner. By travelling on the surfaces
of the room the shortest "straight line" distance from S to F is 10 and
the path is shown on the diagram.

However, there are up to three "shortest" path candidates for any given
cuboid and the shortest route doesn't always have integer length.

It can be shown that there are exactly 2060 distinct cuboids, ignoring
rotations, with integer dimensions, up to a maximum size of M by M by M,
for which the shortest route has integer length when M = 100. This is the
least value of M for which the number of solutions first exceeds two thousand;
the number of solutions when M = 99 is 1975.

Find the least value of M such that the number of solutions first exceeds
one million.
"""


def is_square(x):
    return int(x ** 0.5) ** 2 == x


def find_solution(x):
    solution = 0
    for i in range(1, 2 * x):
        if is_square(i ** 2 + x ** 2):
            if i < x:
                solution += i // 2
            else:
                solution += x - i // 2 + (1 - i % 2)
    return solution


def main():
    # M = 100
    # total_solution = 0
    # for i in range(1, M + 1):
    #     for j in range(i, M + 1):
    #         for k in range(j, M + 1):
    #             if is_square((i+j)**2 + k**2):
    #                 total_solution += 1
    # print(total_solution)

    solution = 0

    for i in range(1, 2000):
        solution += find_solution(i)
        if solution > 1000000:
            print(solution, i)
            break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
