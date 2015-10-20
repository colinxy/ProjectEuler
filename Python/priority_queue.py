"""
use heapq module instead
"""

from math import log2


class MinHeap(object):
    def __init__(self):
        self.heap = []

    def view_heap(self):
        for i in range(int(log2(len(self.heap))) + 1):
            if i < int(log2(len(self.heap))):
                for j in range(2 ** i - 1, 2 ** (i + 1) - 1):
                    print(self.heap[j], end=' ')
                print('\n')
            else:
                for j in range(2 ** i - 1, len(self.heap)):
                    print(self.heap[j], end=' ')

    def extract_min(self):
        min_node = self.heap[0]

        self.heap[0], self.heap[len(self.heap) - 1] = self.heap[len(self.heap) - 1], self.heap[0]
        self.heap.pop()

        i = 0
        #          i: x
        # i*2+1: y     i*2+2: z

        # y in heap range
        while i * 2 + 1 < len(self.heap):
            # z in heap range
            if i * 2 + 2 < len(self.heap):
                # y<x
                if self.heap[i * 2 + 1] < self.heap[i]:
                    # z<y<x
                    if self.heap[i * 2 + 2] < self.heap[i * 2 + 1]:
                        self.heap[i], self.heap[i * 2 + 2] = self.heap[i * 2 + 2], self.heap[i]
                        i = i * 2 + 2
                    # y<x, y<z
                    else:
                        self.heap[i], self.heap[i * 2 + 1] = self.heap[i * 2 + 1], self.heap[i]
                        i = i * 2 + 1
                # z<x<y
                elif self.heap[i * 2 + 2] < self.heap[i]:
                    self.heap[i], self.heap[i * 2 + 2] = self.heap[i * 2 + 2], self.heap[i]
                    i = i * 2 + 2
                # x min
                else:
                    break
            # z not in heap range
            else:
                # y<x
                if self.heap[i * 2 + 1] < self.heap[i]:
                    self.heap[i], self.heap[i * 2 + 1] = self.heap[i * 2 + 1], self.heap[i]
                break

        return min_node

    def insert(self, heap_node):
        self.heap.append(heap_node)
        i = len(self.heap) - 1
        # pop up
        while i > 0:
            if self.heap[i] >= self.heap[(i - 1) // 2]:
                break
            self.heap[i], self.heap[(i - 1) // 2] = self.heap[(i - 1) // 2], self.heap[i]
            i = (i - 1) // 2

"""
heap = MinHeap()
for i in range(100):
    heap.insert(i)
    #heap.view_heap()
    #print(heap.heap)

#heap.view_heap()
for i in range(100):
    print(heap.extract_min())
    #heap.view_heap()
    #print('\n',len(heap.heap))
"""
