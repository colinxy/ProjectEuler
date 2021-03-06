from mathutil import prime_under, product


def generate_prime_tuple(iter_with_len, start_index, level, last, condition):
    result = []
    if level == 1:
        for index in range(start_index, len(iter_with_len)):
            item = iter_with_len[index]
            if not condition(last, item):
                break
            result.append(last + (item, ))
        return result

    for index in range(start_index, len(iter_with_len)):
        item = iter_with_len[index]
        if not condition(last, item):
            break
        curr = generate_prime_tuple(iter_with_len, index+1, level-1, last + (item, ), condition)
        result.extend(curr)

    return result


def generate_prime_prod_apply(iter_with_len, start_index, level, last, condition, apply):
    result = []
    if level == 1:
        for index in range(start_index, len(iter_with_len)):
            item = iter_with_len[index]
            if not condition(last, item):
                break
            result.append(apply(last, item))
        return result

    for index in range(start_index, len(iter_with_len)):
        item = iter_with_len[index]
        if not condition(last, item):
            break
        curr = generate_prime_prod_apply(iter_with_len, index+1, level-1, apply(last, item), condition, apply)
        result.extend(curr)

    return result


def main():
    N = 2 ** 30
    sqrt_N = int(N ** 0.5) + 1
    primes = prime_under(sqrt_N)
    primes_len = len(primes)
    print(primes_len, "primes generated by", time() - starting_time)

    max_num_primes = 0
    prime_product = primes[0]
    while prime_product < sqrt_N:
        max_num_primes += 1
        prime_product *= primes[max_num_primes]
    print("max number of primes", max_num_primes)

    num_square_free = N

    def square_free(start_index, level, last):
        nonlocal num_square_free

        for index in range(start_index, primes_len):
            item = primes[index]
            new_last = last * item
            if new_last >= N:
                break
            num_square_free += N // (new_last * new_last) * (-1)**level
            if level < max_num_primes:
                square_free(index+1, level+1, new_last)

    square_free(0, 1, 1)

    # num_square_free = N
    # flag = 1
    # for num_primes in range(1, max_num_primes+1):
    #     print(num_primes, num_square_free, time() - starting_time)
    #     flag *= -1
    #     # print(generate_prime_tuple(primes, 0, num_primes, (), lambda l, a: product(l) * a < N))
    #     for prime_prod in generate_prime_prod_apply(primes, 0, num_primes, 1, lambda l, a: l*a < N, lambda a, b: a*b):
    #         squared = prime_prod ** 2
    #         num_square_free += N // squared * flag

    print(num_square_free)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    # main()  # infeasible for the actual problem
    print("Time elapsed:", time() - starting_time, "seconds")
