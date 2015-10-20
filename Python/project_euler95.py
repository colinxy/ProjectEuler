N = 1000000
proper_div_sum = [1] * N
encountered = set()


def amicable_chain(n):
    count = 0
    index = 0
    sequence = []
    while True:
        if n == 1 or n > N or n in encountered:
            return 0, n
        try:
            index = sequence.index(n)
            encountered.update(sequence)
            # print(sequence)
            return count - index, min(sequence[index:])
        except ValueError:
            pass

        sequence.append(n)
        count += 1
        n = proper_div_sum[n]


def main():    
    for i in range(2, N):
        if proper_div_sum[i] == 1:  # i is a prime
            for j in range(2*i, N, i):
                num = j
                i_powers = 1
                while num % i == 0:
                    i_powers = i_powers * i + 1
                    num //= i
                proper_div_sum[j] *= i_powers
        else:
            proper_div_sum[i] -= i

    print(max(amicable_chain(num) for num in range(4, N)))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # slow, 15s
    print("Time elapsed:", time() - starting_time, "seconds")
