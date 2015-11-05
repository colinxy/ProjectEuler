# Almost equilateral triangles
"""
It is easily proved that no equilateral triangle exists with 
integral length sides and integral area. However, the
almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle 
for which two sides are equal and the third differs by
no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles 
with integral side lengths and area and whose
perimeters do not exceed one billion (1,000,000,000).
"""

from time import time


def branch1(x, y, z):
    return x - 2 * y + 2 * z, 2 * x - y + 2 * z, 2 * x - 2 * y + 3 * z
def branch2(x, y, z):
    return -x + 2 * y + 2 * z, -2 * x + y + 2 * z, -2 * x + 2 * y + 3 * z
def branch3(x, y, z):
    return x + 2 * y + 2 * z, 2 * x + y + 2 * z, 2 * x + 2 * y + 3 * z


def primitive_pythagorean_gen(triple=(3, 4, 5)):
    return branch1(*triple), branch2(*triple), branch3(*triple)

MAX = 1000000000


def main():
    perimeter_sum = 2 * (3 + 5)

    b1, b2, b3 = primitive_pythagorean_gen()
    stack = [b1, b2, b3]
    while True:
        try:
            current = stack.pop()
        except IndexError:
            break
        if sum(current) <= MAX:
            a, b, c = current
            if abs(c - 2 * min(a, b)) == 1:
                perimeter_sum += 2 * (min(a, b) + c)
            stack.extend(primitive_pythagorean_gen(current))

    print(perimeter_sum)


if __name__ == "__main__":
    starting_time = time()
    main()  # slow, 4 mins
    print("Time elapsed:", time() - starting_time, "seconds")
