from mathutil import prime_under


def pow_mod(power, p):
    """
    power mod for base 10
    in case power is too big

    given that base is 10, and p and 10 are coprime
    """
    e = power % (p - 1)
    return pow(10, e, p)


def repunit_mod(p, degree=9):
    """
    compute R(10 ** degree) % p
    """
    curr_mod = 1111111111 % p
    for i in range(1, degree):
        curr_mod = curr_mod * sum([pow_mod(10 ** i * j, p) for j in range(10)]) % p
    return curr_mod


def main():
    first40 = []
    count = 0
    for p in prime_under(10 ** 6)[3:]:  # exclude 2 and 5, which are not coprime with 10
        if repunit_mod(p) == 0:
            count += 1
            # print(count, p)
            first40.append(p)
            if count == 40:
                break
    # print(first40)
    print(sum(first40))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
