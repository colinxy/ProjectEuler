
def main():
    N = 1000
    r_sigma = [0] * (N+1)
    for a in range(3, N+1):
        if a % 2 == 0:
            r_sigma[a] = a * (a - 2)
        else:
            r_sigma[a] = a * (a - 1)
    # print(r_sigma)
    print(sum(r_sigma))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
