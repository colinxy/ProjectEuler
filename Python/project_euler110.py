from mathutil import prime_under, product
from math import log, ceil


def main():
    N = 4 * 10 ** 6
    N2 = N * 2 + 1
    primes = prime_under(50)
    # print(log(N2, 3), len(primes), list(enumerate(primes)), sep='\n')

    choice = []
    for i3 in range(ceil(log(N2, 3))+1):
        current3 = 3 ** i3
        if current3 > N2:
            choice.append([3]*i3)
            break
        for i5 in range(ceil(log(N2, 5))+1):
            current5 = current3 * 5 ** i5
            if current5 > N2:
                choice.append([3]*i3 + [5]*i5)
                break
            for i7 in range(ceil(log(N2, 7))+1):
                current7 = current5 * 7 ** i7
                if current7 > N2:
                    choice.append([3]*i3 + [5]*i5 + [7]*i7)
                    break
                for i9 in range(ceil(log(N2, 9))+1):
                    current9 = current7 * 9 ** i9
                    if current9 > N2:
                        choice.append([3]*i3 + [5]*i5 + [7]*i7 + [9]*i9)
                        break
                    for i11 in range(ceil(log(N2, 11))+1):
                        current11 = current9 * 11 ** i11
                        if current11 > N2:
                            choice.append([3]*i3 + [5]*i5 + [7]*i7 + [9]*i9 + [11]*i11)
                            break

    # print(len(choice))
    # min_n = 10 ** 20  # inf
    # min_comp = []
    # for seq in choice:
    #     current_n = product(i**(j//2) for i, j in zip(primes, reversed(seq)))
    #     # print(current_n, seq)
    #     if current_n < min_n:
    #         min_n = current_n
    #         min_comp = seq
    # print(min_comp)

    min_n = min(product(i**(j//2) for i, j in zip(primes, reversed(seq))) 
                for seq in choice)
    print(min_n)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time()-starting_time, "seconds")
