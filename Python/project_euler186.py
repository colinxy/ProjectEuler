# -*- coding: utf-8 -*-
# Connectedness of a network
"""
Here are the records from a busy telephone system with one million users:

RecNr	Caller	Called
1	200007	100053
2	600183	500439
3	600863	701497
...	...	...
The telephone number of the caller and the called number in record n are Caller(n) = S2n-1 and Called(n) = S2n
where S1,2,3,... come from the "Lagged Fibonacci Generator":

For 1 ≤ k ≤ 55, Sk = [100003 - 200003k + 300007k3] (modulo 1000000)
For 56 ≤ k, Sk = [Sk-24 + Sk-55] (modulo 1000000)

If Caller(n) = Called(n) then the user is assumed to have misdialled and the call fails; otherwise the call is
successful.

From the start of the records, we say that any pair of users X and Y are friends if X calls Y or vice-versa.
Similarly, X is a friend of a friend of Z if X is a friend of Y and Y is a friend of Z; and so on for longer chains.

The Prime Minister's phone number is 524287. After how many successful calls, not counting misdials, will 99% of the
users (including the PM) be a friend, or a friend of a friend etc., of the Prime Minister?
"""

from collections import deque


class Node(object):
    def __init__(self, parent, rank):
        self.parent = parent
        self.rank = rank
        self.size = 1  # only root is tracking the size of its subtrees


class Graph(object):
    def __init__(self, n, president):
        self.nodes = [Node(i, 0) for i in range(n)]
        self.president = president

    def connect(self, index1, index2):
        self._union(index1, index2)

    def friend_of_president(self):
        return self.nodes[self._find(self.president)].size

    def _parent(self, index):
        return self.nodes[index].parent

    def _root(self, index):  # simply return the root
        parent = self._parent(index)
        while parent != self._parent(parent):
            parent = self._parent(parent)
        return parent

    def _find(self, index):  # doing path compression while going up in the tree
        potentially_root = self._parent(index)
        if potentially_root != index:
            potentially_root = self._find(potentially_root)  # actually_root now
            self.nodes[index].parent = potentially_root
        return potentially_root

    def _union(self, index1, index2):
        s1 = self._find(index1)
        s2 = self._find(index2)
        if s1 == s2:
            return
        if self.nodes[s1].rank > self.nodes[s2].rank:    # s1 is the root
            self.nodes[s2].parent = s1
            self.nodes[s1].size += self.nodes[s2].size
        elif self.nodes[s1].rank < self.nodes[s2].rank:  # s2 is the root
            self.nodes[s1].parent = s2
            self.nodes[s2].size += self.nodes[s1].size
        else:                                            # s1 is the root
            self.nodes[s1].rank += 1
            self.nodes[s2].parent = s1
            self.nodes[s1].size += self.nodes[s2].size


def lag_fibs_gen():
    lag_fibs = deque(maxlen=56)
    for i in range(1, 56):
        current = (100003 - 200003*i + 300007*i**3) % 1000000
        yield current
        lag_fibs.append(current)
    while True:
        current = (lag_fibs[-24] + lag_fibs[-55]) % 1000000
        yield current
        lag_fibs.append(current)


def main():
    N = 10 ** 6
    N99 = N * 99 // 100
    max_calls = 10 ** 8

    network = Graph(N, 524287)
    count = 0
    lag_fibs = iter(lag_fibs_gen())
    for k in range(1, max_calls, 2):
        caller = next(lag_fibs)
        called = next(lag_fibs)
        # print(caller, called)
        if called != caller:
            count += 1
            network.connect(caller, called)
            p_friend = network.friend_of_president()
            # print(k, p_friend)
            if p_friend >= N99:
                break
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # about 20s
    print("Time elapsed:", time() - starting_time, "seconds")
