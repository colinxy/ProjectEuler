def nxt(n):
    if n % 2 == 0:
        return n // 2
    else:
        return n * 3 + 1

collatz_cache = {}


def collatz(n):
    if n == 1:
        return 1
    
    if n in collatz_cache:
        return collatz_cache[n]
    
    collatz_cache[n] = collatz(nxt(n)) + 1
    return collatz_cache[n]
    

def main():
    # max_count = 0
    # max_num = 0
    # for i in range(1, 1000000):
    #     current = collatz(i)
    #     # print(current)
    #     if current > max_count:
    #         max_count = current
    #         max_num = i
    # print(max_count, max_num)
    
    print(max((collatz(i), i) for i in range(1, 1000000)))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed: ", time() - starting_time)
