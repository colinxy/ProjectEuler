
def get_digits(n):
    return list(map(int, str(n)))


def main():
    count = 0
    total = 0
    for i in range(1, 10 ** 7):
        all_digits = get_digits(i)

        total += 1
        increasing = False
        for j in range(1, len(all_digits)):
            if all_digits[j] > all_digits[j-1]:
                increasing = True
                break
        decreasing = False
        for j in range(1, len(all_digits)):
            if all_digits[j] < all_digits[j-1]:
                decreasing = True
                break
        if increasing and decreasing:
            count += 1
        if count / total >= 0.99:
            print(count, i, count / total)
            break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
