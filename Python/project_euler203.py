from mathutil import prime_factorization


def next_pascal(row):
    return [row[0]] + [(row[i-1]+row[i]) for i in range(1, len(row))] + [row[-1]]


def main():
    square_free = {1}
    current = [1]
    for i in range(50):
        current = next_pascal(current)
        # print(current)
        for j in current:
            if all(exp < 2 for exp in prime_factorization(j).values()):
                square_free.add(j)

    print(sum(square_free))
    # print(sorted(square_free))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
