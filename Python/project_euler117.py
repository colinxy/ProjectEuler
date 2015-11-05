def main():
    cache = [0] * 51
    cache[0] = 1
    cache[1] = 1
    cache[2] = 2
    cache[3] = 4
    cache[4] = 8
    for i in range(5, 51):
        cache[i] = cache[i-1] + cache[i-2] + cache[i-3] + cache[i-4]

    print(cache[50])


if __name__ == '__main__':
    main()
