
def main():
    # research
    current = 1777
    s = {1777}
    cache = [1777]
    for i in range(1, 10 ** 8 - 1):
        current = current * 1777 % 10 ** 8
        if current in s:
            break
        s.add(current)
        cache.append(current)
    # print(len(s), current, sep='\n')
    # research conclude that the size of the cycle is 1250000
    # luckily 1250000 is divisible by 10**8

    # upper level solution map 1777**k --> 1777 ** (k % 1250000)
    # cache map k --> (k + 1250000 - 1) % 1250000
    # base level 1777 ** r --> pow(1777, r, 10**8)
    cycle = 1250000
    powers = 1777
    for i in range(1853):
        powers = cache[(powers + cycle - 1) % cycle]

    result = pow(1777, powers, 10 ** 8)
    print(result)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
