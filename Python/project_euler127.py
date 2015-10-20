from mathutil import product, prime_factors_under, prime_under


# def share_factor(a, c):
#     factors_a = pf_cache[a]
#     factors_c = pf_cache[c]
#
#     for p_c in factors_c:
#         for p_a in factors_a:
#             if p_c < p_a:
#                 break
#             if p_c == p_a:
#                 return True
#     return False


def farey_seq(n):
    a, b, c, d = 0, 1, 1, n
    while c / d < 0.5:
        k = int((n + b) / d)
        a, b, c, d = c, d, k*c-a, k*d-b
        yield a, b


def rads3(N, primes, p_len):
    for p_i1 in range(p_len):
        for p_i2 in range(p_i1+1, p_len):
            prod2 = primes[p_i1] * primes[p_i2]
            if prod2 > N:
                break
            for p_i3 in range(p_i2+1, p_len):
                prod3 = prod2 * primes[p_i3]
                if prod3 > N:
                    break
                yield (prod3, [primes[p_i1], primes[p_i2], primes[p_i3]])


def rads4(N, primes, p_len):
    for p_i1 in range(p_len):
        for p_i2 in range(p_i1+1, p_len):
            prod2 = primes[p_i1] * primes[p_i2]
            if prod2 > N:
                break
            for p_i3 in range(p_i2+1, p_len):
                prod3 = prod2 * primes[p_i3]
                if prod3 > N:
                    break
                for p_i4 in range(p_i3+1, p_len):
                    prod4 = prod3 * primes[p_i4]
                    if prod4 > N:
                        break
                    yield (prod4, [primes[p_i1], primes[p_i2], primes[p_i3], primes[p_i4]])


def rads5(N, primes, p_len):
    for p_i1 in range(p_len):
        for p_i2 in range(p_i1+1, p_len):
            prod2 = primes[p_i1] * primes[p_i2]
            if prod2 > N:
                break
            for p_i3 in range(p_i2+1, p_len):
                prod3 = prod2 * primes[p_i3]
                if prod3 > N:
                    break
                for p_i4 in range(p_i3+1, p_len):
                    prod4 = prod3 * primes[p_i4]
                    if prod4 > N:
                        break
                    for p_i5 in range(p_i4+1, p_len):
                        prod5 = prod4 * primes[p_i5]
                        if prod5 > N:
                            break
                        yield (prod5, [primes[p_i1], primes[p_i2], primes[p_i3], primes[p_i4], primes[p_i5]])


def rads6(N, primes, p_len):
    for p_i1 in range(p_len):
        for p_i2 in range(p_i1+1, p_len):
            prod2 = primes[p_i1] * primes[p_i2]
            if prod2 > N:
                break
            for p_i3 in range(p_i2+1, p_len):
                prod3 = prod2 * primes[p_i3]
                if prod3 > N:
                    break
                for p_i4 in range(p_i3+1, p_len):
                    prod4 = prod3 * primes[p_i4]
                    if prod4 > N:
                        break
                    for p_i5 in range(p_i4+1, p_len):
                        prod5 = prod4 * primes[p_i5]
                        if prod5 > N:
                            break
                        for p_i6 in range(p_i5+1, p_len):
                            prod6 = prod5 * primes[p_i6]
                            if prod6 > N:
                                break
                            yield (prod6, [primes[p_i1], primes[p_i2], primes[p_i3], primes[p_i4], primes[p_i5], primes[p_i6]])


def rads7(N, primes, p_len):
    for p_i1 in range(p_len):
        for p_i2 in range(p_i1+1, p_len):
            prod2 = primes[p_i1] * primes[p_i2]
            if prod2 > N:
                break
            for p_i3 in range(p_i2+1, p_len):
                prod3 = prod2 * primes[p_i3]
                if prod3 > N:
                    break
                for p_i4 in range(p_i3+1, p_len):
                    prod4 = prod3 * primes[p_i4]
                    if prod4 > N:
                        break
                    for p_i5 in range(p_i4+1, p_len):
                        prod5 = prod4 * primes[p_i5]
                        if prod5 > N:
                            break
                        for p_i6 in range(p_i5+1, p_len):
                            prod6 = prod5 * primes[p_i6]
                            if prod6 > N:
                                break
                            for p_i7 in range(p_i6, p_len):
                                prod7 = prod5 * primes[p_i7]
                                if prod7 > N:
                                    break
                                yield (prod7, [primes[p_i1], primes[p_i2], primes[p_i3], primes[p_i4], primes[p_i5], primes[p_i6], primes[p_i7]])


def main():
    N = 120000

    primes = prime_under(N)
    primes_len = len(primes)
    pf_cache = [[0], [1]]
    pf_cache.extend(prime_factors_under(N))
    print("preprocessing finished", time() - starting_time, "seconds")

    abc_hit = []

    # 2: a = 1
    for i in range(2, len(pf_cache)):  # exclude 0, 1
        if len(pf_cache[i]) == 1 and len(pf_cache[i-1]) == 1 and pf_cache[i][0] * pf_cache[i-1][0] < i:
            abc_hit.append(i)

    # 3
    print(len(list(rads3(N, primes, primes_len))))
    for prod, ps in rads3(N, primes, primes_len):
        pass

    # 4
    print(len(list(rads4(N, primes, primes_len))))

    # 5
    print(len(list(rads5(N, primes, primes_len))))

    # 6
    print(len(list(rads6(N, primes, primes_len))))

    # for a, c in prod_cache:
    #     if prod_cache[a] * prod_cache[c-a] * prod_cache[c] < c:
    #         abc_hit.append(c)
    #         # print(a, c-a, c, pf_cache[c])
    print(len(abc_hit))
    print(sum(abc_hit))
    print(abc_hit)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    print("broken")
    # main()
    print("Time elapsed:", time() - starting_time, "seconds")
