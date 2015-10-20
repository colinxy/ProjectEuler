class Cache:
    def __init__(self, n):
        self.cache = [0] * (n + 1)
        self.cache[0] = 1

    def get_cache(self, k):
        if k < -1:
            return 0
        elif k == -1:
            return 1
        return self.cache[k]

    def set_cache(self, k, value):
        self.cache[k] = value


def main():
    c = Cache(50)
    for i in range(1, 51):
        value = c.get_cache(i-1) + sum([c.get_cache(i-j-1) for j in range(3, i+1)])
        c.set_cache(i, value)

    # print(c.cache)
    print(c.get_cache(50))


if __name__ == '__main__':
    main()
