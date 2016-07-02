# project euler 233: Lattice points on a circle

from mathutil import prime_factorization


def farey(n):
    fareys = []
    a, b, c, d = 0, 1, 1, n
    while c < n:
        k = (n + b) // d
        a, b, c, d = c, d, k*c - a, k*d - b

        if (a - b) & 1:
            print(a, b)
            fareys.append((a*a + b*b, a, b))
    return fareys


def main():
    coprime_pairs = farey(100)
    coprime_pairs.sort()
    for cand in coprime_pairs:
        print(cand, sorted(prime_factorization(cand[0]).items()))


if __name__ == '__main__':
    main()
