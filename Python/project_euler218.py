# project euler 218: Perfect right-angled triangles

"""
perfect right angled triangle:
    primitive
    hypotenuse is a perfect square

super-perfect right angled triangle:
    perfect
    area is multiple of 6, 28


!!! can be proved that no such triangle exists

https://luckytoilet.wordpress.com/2010/06/20/on-some-number-theoretic-properties-of-right-triangles-project-euler-218/


a**2 + b**2 = c**2

a = m**2 - n**2
b = 2*m*n
c = m**2 + n**2

m = p**2 - q**2
n = 2*p*q
c = (p**2 + q**2) ** 2
b = 4*p*q*(p**2 - q**2)
a = p**4 + q**4 - 6 * p**2 * q**2
"""

from fractions import gcd

N = 10 ** 4
SIZE = 10 ** 16


def main():
    count = 0
    for i in range(1, N):
        # i, j of different parity
        for j in range(i + 1, N, 2):
            if gcd(i, j) == 1:
                c = (i**2 + j**2) ** 2
                if c > SIZE:
                    break

                b = 4 * i * j * (j**2 - i**2)
                a = i**4 + j**4 - 6 * i**2 * j**2

                if (a * b) % 168 != 0:
                    print(a, b, c)
                    count += 1

    print(count)


if __name__ == '__main__':
    main()
