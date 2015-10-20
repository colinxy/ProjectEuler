INF = 10000000


def main():
    cache = {}
    length = 0
    final_length = None
    for i in range(INF):
        perm = ''.join(sorted(str(i ** 3)))
        current_length = len(perm)
        if final_length is not None:
            if current_length > final_length:
                break

        if current_length > length:
            length = current_length
            cache.clear()

        cache[perm] = cache.get(perm, []) + [i]
        if len(cache[perm]) >= 5:
            print(cache[perm], cache[perm][0] ** 3)
            final_length = length


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
