import numpy as np
import itertools as it


class LogicGraph:
    def __init__(self, n):
        self.truth_table = np.zeros(2 ** n, dtype=np.bool)
        self.graph = []
        for i in range(2 ** n):
            self.graph.append([])

    def add_edge(self, p, q):
        self.graph[p].append(q)

    def traverse(self, start):
        traversed = {start}
        trip = [start]
        this = start
        while True:
            this = self.graph[this][0]
            if this not in traversed:
                traversed.add(this)
                trip.append(this)
            else:
                break
        return trip

    def __str__(self):
        string_rep = []
        for index, item in enumerate(self.graph):
            string_rep.append(str(index) + ': ' + str(item))
        return '\n'.join(string_rep)


def main():
    lg = LogicGraph(6)
    for i in it.product('01', repeat=6):
        a, b, c = map(int, i[:3])
        s = ''.join(i)
        p = int(s, 2)
        q = int(s[1:] + str(a ^ (b & c)), 2)
        lg.add_edge(p, q)
    # print(lg, '\n')

    t1 = lg.traverse(1)
    t2 = lg.traverse(3)
    t3 = lg.traverse(5)
    t4 = lg.traverse(9)
    t5 = lg.traverse(21)
    temp = sorted(t1 + t2 + t3 + t4 + t5)
    print(temp == list(range(1, 64)), len(temp))
    print(t1, len(t1))
    print(t2, len(t2))
    print(t3, len(t3))
    print(t4, len(t4))
    print(t5, len(t5), '\n')

    # research concludes that there are 4 loops, length: 6, 46, 6, 3, 2
    # DP to nail it

    cache0 = np.zeros((2, 64), dtype=np.int64)  # initial start 0
    cache0[0, 1] = 1
    cache0[1, 1] = 0
    for i in range(2, 50):
        cache0[0, i] = cache0[0, i-1] + cache0[1, i-1]
        cache0[1, i] = cache0[0, i-1]
    cache1 = np.zeros((2, 64), dtype=np.int64)  # initial start 1
    cache1[0, 1] = 0
    cache1[1, 1] = 1
    for i in range(2, 50):
        cache1[0, i] = cache1[0, i-1] + cache1[1, i-1]
        cache1[1, i] = cache1[0, i-1]

    result = 1
    for i in [6, 46, 6, 3, 2]:
        result *= (cache0[0, i] + cache0[1, i] + cache1[0, i])
    print(result)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time()-starting_time, "seconds")
