# -*- coding: utf-8 -*-

# Prime digit replacements
"""
By replacing the 1st digit of the 2-digit number *3, it turns out 
that six of the nine possible values: 13, 23, 43, 53,
73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, 
this 5-digit number is the first example having seven primes among 
the ten generated numbers, yielding the family: 56003, 56113, 56333, 
56443, 56663, 56773, and 56993.
Consequently 56003, being the first member of this family, is 
the smallest prime with this property.

Find the smallest prime which, by replacing part of the number 
(not necessarily adjacent digits) with the same digit,
is part of an eight prime value family.
"""

from time import time
from mathutil import is_prime


def prime_cluster(rule, number):
    rule = int(rule)
    cluster = [number + rule * i for i in range(10)]

    count = sum(is_prime(i) for i in cluster)
    if count >= 8:
        print(count)
        for item in cluster:
            if is_prime(item):
                print(item)

    return count


def number_generator(rule):
    digits = rule.count('0')
    numbers = []

    for i in range(10**(digits-1)+1, 10**digits, 2):
        number_list = []
        number_list.extend(str(i))
        for index in range(len(rule)):
            if rule[index] == '1':
                number_list.insert(index, '0')

        number = int(''.join(number_list))
        numbers.append(number)

    return numbers


def replace(n):
    sequence = []
    # n digit numbers
    for i in range(1, 2**(n-1)):
        s = bin(i)
        s = s.lstrip('0b')
        s += '0'
        sequence.append(s)

    results = []
    for i in sequence:
        for j in number_generator(i):
            if prime_cluster(i, j) >= 8:
                results.append((i, j))

    return results


def main():
    for i in range(5, 8):
        current = replace(i)
        if len(current) > 0:
            # print(current)
            break
        # print("Time elapsed for", i, "digit number:", time() - starting_time, "seconds")
    

if __name__ == "__main__":
    starting_time = time()
    main()
    print("Total time elapsed:", time() - starting_time, "seconds")
