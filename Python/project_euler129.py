def A(n):
    k = 1
    repunit = 1
    while repunit != 0:
        k += 1
        repunit = (repunit * 10 + 1) % n
    return k


def main():
    N = 1000000

    n = N
    if N % 2 == 0:
        n += 1

    for n in range(n, 2*n, 2):
        if n % 5 == 0:
            continue
        a_n = A(n)
        if a_n > N:
            print(n, a_n)
            break
        # print(n, a_n)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
