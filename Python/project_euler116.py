def main():
    cache2 = [0] * 51
    cache2[0] = 1
    cache2[1] = 1
    for i in range(2, 51):
        cache2[i] = cache2[i-1] + cache2[i-2]

    cache3 = [0] * 51
    cache3[0] = 1
    cache3[1] = 1
    cache3[2] = 1
    for i in range(3, 51):
        cache3[i] = cache3[i-1] + cache3[i-3]

    cache4 = [0] * 51
    cache4[0] = 1
    cache4[1] = 1
    cache4[2] = 1
    cache4[3] = 1
    for i in range(4, 51):
        cache4[i] = cache4[i-1] + cache4[i-4]

    # print(cache2, cache3, cache4, sep='\n')
    print(cache2[50]-1 + cache3[50]-1 + cache4[50]-1)


if __name__ == '__main__':
    main()
