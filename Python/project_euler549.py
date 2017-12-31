# Problem 549: Divisibility of factorials

from __future__ import division

N = 10**8


def prime_factorization_under(ceiling):
    factor = [{} for _ in range(ceiling)]
    for i in range(2, ceiling):
        if not factor[-1]:
            factor[-1][i] = 1
            yield factor.pop()
            for j in range(-i, -len(factor), -i):
                factor[j][i] = 1
        else:
            for fac in factor[-1]:
                number = i // fac
                while number % fac == 0:
                    factor[-1][fac] += 1
                    number //= fac
            yield factor.pop()


facts = None


def s(n):
    global facts
    if facts is None:
        facts = list(prime_factorization_under(N+1))

    m = 0
    for f, c in facts[n-2].items():
        for i in range(f, n+1, f):
            c -= facts[i-2][f]
            if c <= 0:
                m = max(m, i)
                break
    return m


def s_(n, facts):
    m = 0
    for f in facts:
        count = 0
        while n % f == 0:
            count += 1
            n //= f

        mult = 0
        cumulative = 0
        while cumulative < count:
            mult += 1
            cumulative += 1 + count_factors(mult, f)

        m = max(m, mult*f)
    return m


def count_factors(n, fact):
    count = 0
    while n % fact == 0:
        count += 1
        n //= fact
    return count


def S(n):
    divs = [0] * (n+1)

    for i in range(2, n+1):
        if not divs[i]:
            exp = 1
            i_exp = i
            mult = 0
            cumulative = 0
            while i_exp <= n:
                if cumulative < exp:
                    mult += 1
                    cumulative += 1 + count_factors(mult, i)

                for j in range(i_exp, n+1, i_exp):
                    divs[j] = max(divs[j], mult * i)

                exp += 1
                i_exp *= i

    return sum(divs)


def main():
    # print(sum([s(i) for i in range(2, N+1)]))

    print(S(N))


if __name__ == '__main__':
    main()
