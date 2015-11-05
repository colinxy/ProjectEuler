from mathutil import is_prime
from itertools import permutations, combinations


def get_prime(min_num, digits_left):
    digits_left_len = len(digits_left)
    min_size_len = len(str(min_num))

    for i in range(min_size_len, digits_left_len//2+1):
        for comb in combinations(digits_left, i):
            for perm in permutations(comb):
                num = int(''.join(perm))
                if num <= min_num:
                    continue
                if is_prime(num):
                    yield num


# the sets terminate here
def get_prime_pan(curr, min_num, digits_left):
    for perm in permutations(digits_left):
        num = int(''.join(perm))
        if num <= min_num:
            continue
        if is_prime(num):
            pan_primes = tuple(curr) + (num, )
            # print(pan_primes)
            pan_prime_sets.append(pan_primes)


def pick_num(curr, min_num, digits_left):
    # safety termination clause
    if len(digits_left) == 0:
        raise Exception("impossible")

    if int(''.join(sorted(digits_left, reverse=True))) <= min_num:
        return

    if len(str(min_num)) <= len(digits_left)//2:
        for prime in get_prime(min_num, sorted(digits_left)):
            # print(prime)
            pick_num(curr+[prime], prime, digits_left-set(str(prime)))

    if sum(map(int, digits_left)) % 3 != 0:
        get_prime_pan(curr, min_num, sorted(digits_left))


pan_prime_sets = []
digits = {'1', '2', '3', '4', '5', '6', '7', '8', '9'}

def main():
    pick_num([], 0, digits)

    # print(check_pan_prime(pan_prime_sets))
    # for i in pan_prime_sets: print(i)
    print(len(pan_prime_sets))
    # print(len({tuple(sorted(ps)) for ps in pan_prime_sets}))
    # print(pan_prime_sets.index((2, 5, 47, 89, 631)))


def check_pan_prime(list_list_primes):
    return all( all(map(is_prime, p_list)) and (set(''.join(map(str, p_list))) == digits and len(''.join(map(str, p_list))) == 9) for p_list in list_list_primes)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
