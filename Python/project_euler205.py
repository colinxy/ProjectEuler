from fractions import Fraction


def increment(case, incre):
    changed = {}
    for i in case:
        for j in incre:
            changed[i+j] = changed.get(i+j, 0) + case[i] * incre[j]
    return changed


def main():
    base4 = {i: Fraction(1, 4) for i in range(1, 5)}
    base6 = {i: Fraction(1, 6) for i in range(1, 7)}

    case4 = base4
    for i in range(8):
        case4 = increment(case4, base4)
    case6 = base6
    for i in range(5):
        case6 = increment(case6, base6)
    # print(case4, case6, sep='\n\n')

    prob = 0
    for i in range(36, 8, -1):
        prob += case4[i] * sum(case6.get(j, 0) for j in range(i-1, 5, -1))
    print(float(prob))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
