from mathutil import product, prime_factorization_under, prime_factorization


# work through the math to see why
def reciprocal_solution(n):
    # return sum(n*i % (i-n) == 0 for i in range(n+1, 2*n+1))  # slow
    return (product(2*i+1 for i in prime_factorization(n).values()) + 1) // 2


def main():
    for i, factors in enumerate(prime_factorization_under(10**6)):
        number = i + 2
        num_solution = (product(2*i+1 for i in factors.values()) + 1) // 2
        # print(number, num_solution)
        if num_solution > 1000:
            print(number, num_solution)
            break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    try:
        main()
    except MemoryError:
        print("Memory Error")
    print("Time elapsed:", time() - starting_time, "seconds")
