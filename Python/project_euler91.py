
def right_tri(x1, y1, x2, y2):
    s1, s2, s3 = sorted([x1**2+y1**2, x2**2+y2**2, (x1-x2)**2+(y1-y2)**2])
    return s1 + s2 == s3


def grid(N):
    for i in range(0, N+1):
        for j in range(0, N+1):
            if not (i == 0 and j == 0):
                yield i, j


def main():
    N = 50
    grids = list(grid(N))
    # print(len(grids))

    count = 0
    for i in range(0, len(grids)):
        for j in range(i+1, len(grids)):
            x1, y1 = grids[i]
            x2, y2 = grids[j]
            if right_tri(x1, y1, x2, y2):
                count += 1
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
