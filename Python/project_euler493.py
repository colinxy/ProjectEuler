# Under The Rainbow
"""
70 colored balls are placed in an urn, 10 for each of the seven rainbow colors.

What is the expected number of distinct colors in 20 randomly picked balls?

Give your answer with nine digits after the decimal point (a.bcdefghij).
"""

"""
distinct_colors = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0, 7: 0}  # 1-7 stand for the number of distinctly picked colors
each = 4


def pick_num_of_color(picks, ball_picked, left_state, colors):
    if ball_picked == picks:
        if len([i for i in left_state if left_state[i] < each]) == colors:
            distinct_colors[colors] += 1
        # print(count, distinct_color_num)
        return

    ball_picked += 1
    for i in range(1, colors+1):
        if left_state[i] > 0:
            this_state = left_state.copy()
            this_state[i] -= 1
            pick_num_of_color(picks, ball_picked, this_state, colors)
            # print(this_state)


def pick(picks, initial_state):
    for i in range(2, 8):
        pick_num_of_color(picks, 0, initial_state, i)
        distinct_colors[i] *= nCr(7, i)"""


"""
    picks = 8
    initial_state = {1: each, 2: each, 3: each, 4: each, 5: each, 6: each, 7: each}  # 1-7 stand for seven diff colors
    pick(picks, initial_state)
    count = sum(distinct_colors.values())

    print(distinct_colors)
    print(count)
    expectation = sum(i*j for i, j in distinct_colors.items()) / count
    print(expectation)"""


from functools import reduce
from operator import mul


def nCr(m, n):
    result = 1
    for i in range(m, m-n, -1):
        result *= i
    for i in range(n, 1, -1):
        result //= i
    return result


def nPr(m, n):
    result = 1
    for i in range(m, m-n, -1):
        result *= i
    return result


def deal_with_duplicate(*args):
    """
    e.g. 1,3,3,5,5,8,8,8 -> 1!*2!*2!*3!
    """
    return product(nPr(args.count(i), args.count(i)) for i in set(args))


def product(nums):
    return reduce(mul, nums)


def duplicates(*args):
    return product(nCr(N, i) for i in args)


N = 10


def main():
    print("A genius solution")
    print((1 - nCr(60, 20) / nCr(70, 20)) * 7, end='\n\n')

    print("Another elegant but slightly slow solution")
    total=0
    for a in range(11):
        for b in range(min(21-a,11)):
            for c in range(min(21-a-b,11)):
                for d in range(min(21-a-b-c,11)):
                    for e in range(min(21-a-b-c-d,11)):
                        for f in range(min(21-a-b-c-d-e,11)):
                            g=20-a-b-c-d-e-f
                            total+=nCr(10,a)*nCr(10,b)*\
                                   nCr(10,c)*nCr(10,d)*\
                                   nCr(10,e)*nCr(10,f)*\
                                   nCr(10,g)*\
                                   sum([1 for x in (a,b,c,d,e,f,g) if x>0])
    print(float(total)/nCr(70,20), end='\n\n')

    print(nCr(7*N, 2*N))

    colors = {1: 0, 2: nCr(7, 2), 3: 0, 4: 0, 5: 0, 6: 0, 7: 0}
    # 3 colors
    for i in range(1, N):
        for j in range(i, N):
            k = 2*N - i - j
            if k < j:
                break
            colors[3] += nPr(7, 3) // deal_with_duplicate(i, j, k) * duplicates(i, j, k)
    # 4
    for i in range(1, N):
        for j in range(i, N):
            for k in range(j, N):
                l = 2*N - i - j - k
                if l < k:
                    break
                colors[4] += nPr(7, 4) // deal_with_duplicate(i, j, k, l) * duplicates(i, j, k, l)
    # 5
    for i in range(1, N):
        for j in range(i, N):
            for k in range(j, N):
                for l in range(k, N):
                    m = 2*N - i - j - k - l
                    if m < l:
                        break
                    colors[5] += nPr(7, 5) // deal_with_duplicate(i, j, k, l, m) * duplicates(i, j, k, l, m)
    # 6
    for i in range(1, N):
        for j in range(i, N):
            for k in range(j, N):
                for l in range(k, N):
                    for m in range(l, N):
                        n = 2*N - i - j - k - l - m
                        if n < m:
                            break
                        colors[6] += nPr(7, 6) // deal_with_duplicate(i, j, k, l, m, n) * duplicates(i, j, k, l, m, n)
    # 7
    for i in range(1, N):
        for j in range(i, N):
            for k in range(j, N):
                for l in range(k, N):
                    for m in range(l, N):
                        for n in range(m, N):
                            o = 2*N - i - j - k - l - m - n
                            if o < n:
                                break
                            colors[7] += nPr(7, 7) // deal_with_duplicate(i, j, k, l, m, n, o) * duplicates(i, j, k, l, m, n, o)

    print(colors)
    total = sum(colors.values())
    print(total)
    print(sum(i * j for i, j in colors.items()) / total)


if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
