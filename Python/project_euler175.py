from project_euler169 import sum_power, group_solve
from itertools import product
from sympy import symbols, expand


def add1(groups_in_int):
    if groups_in_int[-1] != 1:
        return groups_in_int[:-1] + [groups_in_int[-1] - 1] + [1]
    i = -2
    while groups_in_int[i] == 1:
        i -= 1
    return groups_in_int[:i] + [groups_in_int[i] - 1] + [-i]


def main():
    print(symbols('a b c d'))
    a, b, c, d = symbols('a b c d')

    group4 = [a, b, c, d]
    print(group4)
    four = group_solve(group4)
    print(four)
    print(expand(four))
    print(four.collect(a))
    print(four.subs(a, 10))
    print()

    group4_ = add1(group4)
    print(group4_)
    four1 = group_solve(group4_)
    print(four1)
    print(expand(four1))
    print(four1.collect(a))
    print()

    print(13717421)

    for i in range(1, 10 ** 4):
        sum_power(i)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
