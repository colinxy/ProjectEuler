
def s_repunit_of_base(base, limit):
    total = 0
    last = base*base + base + 1
    while last < limit:
        total += last
        last = last * base + 1
    return total


def gen(base, limit, start=1):
    result = []
    last = (base ** start - 1) // (base - 1)
    while last < limit:
        result.append(last)
        last = last * base + 1
    return result


def main():
    N = 10 ** 12
    top_base = int(N ** 0.5)

    strong_repunit = {1}
    for base in range(2, top_base+1):
        strong_repunit.update(gen(base, N, 3))
    print(sum(strong_repunit))
    # print(len(strong_repunit))

    # incorrect, cannot eliminate duplicate
    # print(sum(s_repunit_of_base(base, N) for base in range(2, top_base+1)) + 1 - 31)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
