# -*- coding: utf-8 -*-

# Pandigital products
"""
We shall say that an n-digit number is pandigital if it makes use of 
all the digits 1 to n exactly once; for example, the 5-digit number, 
15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product 
identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure 
to only include it once in your sum.
"""

from time import time
tstart = time()


def is_pan(s): 
    return True if set(str(s)) == set('123456789') and len(str(s)) <= 9 else False


def main():
    product_set = set()
    
    for multiplicand in range(12, 99):
        for multiplier in range(102, 988):
            product = multiplicand * multiplier
            if product < 10000:
                if is_pan(str(multiplicand) + str(multiplier) + str(product)):
                    product_set.add(product)
                    # print(product, multiplicand, multiplier)

    for multiplicand in range(2, 10):
        for multiplier in range(1002, 4988):
            product = multiplicand * multiplier
            if product < 10000:
                if is_pan(str(multiplicand) + str(multiplier) + str(product)):
                    product_set.add(product)
                    # print(product, multiplicand, multiplier)

    print(sum(product_set))


if __name__ == '__main__':
    main()
    print("Time elapsed:", time()-tstart, 'seconds')
