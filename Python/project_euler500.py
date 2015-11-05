from mathutil import prime_under, product_mod
import heapq


def main():
    primes = prime_under(10 ** 7)
    # print(len(primes), time() - starting_time)
    factors = [2]
    frontier = [4]
    heapq.heapify(frontier)

    prime_index = 1  # 2 already included
    for i in range(500500 - 1):
        if frontier[0] >= primes[prime_index]:
            # push square of the current prime into the queue
            heapq.heappush(frontier, primes[prime_index] ** 2)
            factors.append(primes[prime_index])
            prime_index += 1
        else:
            minimum = heapq.heappop(frontier)
            heapq.heappush(frontier, minimum ** 2)
            factors.append(minimum)

    # print(factors)
    print(product_mod(factors, 500500507))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
