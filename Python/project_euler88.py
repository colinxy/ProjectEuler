# Product-sum numbers
"""
A natural number, N, that can be written as the sum and product of 
a given set of at least two natural numbers, {a1, a2, ... , ak} 
is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with 
this property a minimal product-sum number. The minimal product-sum 
numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

k=2: 4 = 2 × 2 = 2 + 2
k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2≤k≤6, the sum of all the minimal product-sum numbers 
is 4+6+8+12 = 30; note that 8 is only counted once
in the sum.

In fact, as the complete set of minimal product-sum numbers for 
2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.

What is the sum of all the minimal product-sum numbers for 2≤k≤12000?
"""

import numpy as np


def factorization(n, start=2):
    if start * start > n:
        return [[n]]

    the_factorization = [[n]]
    for i in range(start, int(n ** 0.5) + 1):
        if n % i == 0:
            fac = factorization(n // i, i)
            for each in fac:
                each.append(i)
            the_factorization.extend(fac)

    return the_factorization


def main():
    k = 12000

    # solution using numpy array, about 20s
    value_matrix = np.zeros((k+1, 2*k+1), dtype=np.bool)
    product_sums = np.zeros(k+1, dtype=np.int32)

    for i in range(2, 2*k+1):
        for j in factorization(i):
            k_val = i - sum(j) + len(j)
            if 0 <= k_val <= k:
                value_matrix[k_val, i] = True
    print("Factorization finished by:", time() - starting_time, "seconds")
    for k_val in range(2, k+1):
        for i in range(2, 2*k+1):
            if value_matrix[k_val, i]:
                product_sums[k_val] = i
                break
    print(sum(set(product_sums)))

    # # solution using python dict, about 50s
    # product_sum_dict = {}
    # number_dict = {}

    # for i in range(2, 2 * k + 1):
    #     number_dict[i] = [(i - sum(j) + len(j)) for j in factorization(i) if (i - sum(j) + len(j)) >= 0]
    # # i in number_dict: print(i, number_dict[i])
    # print("Factorization finished by:", time() - starting_time, "seconds")

    # for k_val in range(2, k + 1):
    #     for i in sorted(number_dict.keys()):
    #         if k_val in number_dict[i]:
    #             product_sum_dict[k_val] = i
    #             break
    # # print(product_sum_dict)

    # numbers = set(product_sum_dict.values())
    # # print(sorted(list(numbers)))
    # print(sum(numbers))


if __name__ == "__main__":
    from time import time

    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
