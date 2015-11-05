# SLOW
# Singular integer right triangles
"""
It turns out that 12 cm is the smallest length of wire that 
can be bent to form an integer sided right angle triangle
in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent 
to form an integer sided right angle triangle, and other
lengths allow more than one solution to be found; for example, 
using 120 cm it is possible to form exactly three
different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values 
of L ≤ 1,500,000 can exactly one integer sided right angle
triangle be formed?
"""

from time import time
L = 1500000
triples = []

branch1 = lambda x, y, z: (x - 2 * y + 2 * z, 2 * x - y + 2 * z, 2 * x - 2 * y + 3 * z)
branch2 = lambda x, y, z: (-x + 2 * y + 2 * z, -2 * x + y + 2 * z, -2 * x + 2 * y + 3 * z)
branch3 = lambda x, y, z: (x + 2 * y + 2 * z, 2 * x + y + 2 * z, 2 * x + 2 * y + 3 * z)


def primitive_pythagorean_triple_generator(max_sum, a=3, b=4, c=5):    
    a1, b1, c1 = branch1(a, b, c)
    if a1 + b1 + c1 <= max_sum:
        triples.append((a1, b1, c1))
        primitive_pythagorean_triple_generator(max_sum, a1, b1, c1)

    a2, b2, c2 = branch2(a, b, c)
    if a2 + b2 + c2 <= max_sum:
        triples.append((a2, b2, c2))
        primitive_pythagorean_triple_generator(max_sum, a2, b2, c2)

    a3, b3, c3 = branch3(a, b, c)
    if a3 + b3 + c3 <= max_sum:
        triples.append((a3, b3, c3))
        primitive_pythagorean_triple_generator(max_sum, a3, b3, c3)


"""def is_right_singular(n, sums):
    i = 0
    factor_count = 0
    while i < len(sums) and sums[i] <= n:
        if n % sums[i] == 0:
            factor_count += 1
            if factor_count > 1:
                return False
        i += 1

    if factor_count == 1:
        return True
    else:
        return False"""


def main():
    triples.append((3, 4, 5))
    primitive_pythagorean_triple_generator(L, 3, 4, 5)
    # print("primitive pythagorean triple generated:", time() - starting_time, "seconds")

    """with open("primitive_pythagorean_triple.txt", 'w') as f:
        for triple in triples:
            f.write(str(triple[0]) + ' ' + str(triple[1]) + ' ' + str(triple[2]) + '\n')"""

    triple_sums = [sum(triple) for triple in triples]
    """triple_sums.sort()
    for triple_sum in triple_sums:
        print(triple_sum)"""

    all_triple_sums = []
    for triple_sum in triple_sums:
        for i in range(1, L // triple_sum + 1):
            all_triple_sums.append(triple_sum * i)
    all_triple_sums.sort()

    count = 1  # 12 included, 1500000 excluded
    for index in range(1, len(all_triple_sums) - 1):
        if all_triple_sums[index] != all_triple_sums[index + 1] and \
                all_triple_sums[index] != all_triple_sums[index - 1]:
            count += 1
            # print(all_triple_sums[index])
    print(count)


if __name__ == "__main__":
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
