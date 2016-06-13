# for this problem, I am seriously indebted to Daotong Ge,
# who gave the insight that x_1:x_2:..:x_m = 1:2:..:m


def max_weight_prod(m):
    prod = 1
    for i in range(1, m+1):
        prod *= (2 / (1+m) * i) ** i

    return prod


def main():
    # for m in range(2, 16):
    #     print(m, max_weight_prod(m))
    print(sum(int(max_weight_prod(m)) for m in range(2, 16)))


if __name__ == '__main__':
    main()
