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


def F(m, n):
    c = Cache(n)
    for i in range(1, n+1):
        value = c.get_cache(i-1) + sum([c.get_cache(i-j-1) for j in range(m, i+1)])
        c.set_cache(i, value)

    # print(c.cache)
    # print(c.get_cache(n))
    return c.get_cache(n)


def main():
    for i in range(50, 10000):
        f = F(50, i)
        if f > 1000000:
            print("f:", f, "F(50, {0})".format(i))
            break


if __name__ == '__main__':
    main()
