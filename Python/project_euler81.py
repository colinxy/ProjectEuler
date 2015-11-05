# Path sum: two ways
"""
In the 5 by 5 matrix below, the minimal path sum from the top left 
to the bottom right, by only moving to the right and
down, is indicated in bold red and is equal to 2427.

Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by
80 matrix, from the top left to the bottom right by only moving right and down.
"""

from math import log2
from time import time
starting_time = time()
INF = 1000000000
# each node is represented with three data points: 
# length, current path sum(INF if unexplored), explored


class Node(object):
    def __init__(self, length, greedy_score, explored):
        self.length = length
        self.greedy_score = greedy_score
        self.explored = explored


class Matrix(object):
    def __init__(self):
        self.nodes = []

    def view_matrix(self):
        for i in self.nodes:
            for j in i:
                print(j.length, end=' ')
            print('\n')

    def add_row(self, row):
        self.nodes.append(row)

    def goto_index(self, index):
        return self.nodes[index[0]][index[1]]

    def next_nodes(self, start):
        next_node1, next_node2 = None, None
        if start[0] + 1 < len(self.nodes):
            next_node1 = (start[0] + 1, start[1])
        if start[1] + 1 < len(self.nodes[start[0]]):
            next_node2 = (start[0], start[1] + 1)
        return next_node1, next_node2


class HeapNode(object):
    def __init__(self, greedy_score, node_index):
        self.greedy_score = greedy_score
        self.node_index = node_index


class Heap(object):
    def __init__(self):
        self.heap = []

    def view_heap(self):
        for i in range(int(log2(len(self.heap)))+1):
            if i < int(log2(len(self.heap))):
                for j in range(2**i-1, 2**(i+1)-1):
                    print(self.heap[j].greedy_score, end=' ')
                print('\n')
            else:
                for j in range(2**i-1, len(self.heap)):
                    print(self.heap[j].greedy_score, end=' ')

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
                if self.heap[i * 2 + 1].greedy_score < self.heap[i].greedy_score:
                    # z<y<x
                    if self.heap[i * 2 + 2].greedy_score < self.heap[i * 2 + 1].greedy_score:
                        self.heap[i], self.heap[i * 2 + 2] = self.heap[i * 2 + 2], self.heap[i]
                        i = i * 2 + 2
                    # y<x, y<z
                    else:
                        self.heap[i], self.heap[i * 2 + 1] = self.heap[i * 2 + 1], self.heap[i]
                        i = i * 2 + 1
                # z<x<y
                elif self.heap[i * 2 + 2].greedy_score < self.heap[i].greedy_score:
                    self.heap[i], self.heap[i * 2 + 2] = self.heap[i * 2 + 2], self.heap[i]
                    i = i * 2 + 2
                # x min
                else:
                    break
            # z not in heap range
            else:
                # y<x
                if self.heap[i * 2 + 1].greedy_score < self.heap[i].greedy_score:
                    self.heap[i], self.heap[i * 2 + 1] = self.heap[i * 2 + 1], self.heap[i]
                break

        return min_node

    def insert(self, heap_node):
        self.heap.append(heap_node)
        i = len(self.heap) - 1
        # pop up
        while i > 0:
            if self.heap[i].greedy_score >= self.heap[(i - 1) // 2].greedy_score:
                break
            self.heap[i], self.heap[(i - 1) // 2] = self.heap[(i - 1) // 2], self.heap[i]
            i = (i - 1) // 2

    def add_heap_node(self, greedy_score, node_index):
        heap_node = HeapNode(greedy_score, node_index)
        self.insert(heap_node)

    def heapify(self, graph, start):
        next_node1, next_node2 = graph.next_nodes(start)
        if next_node1 is not None and not graph.goto_index(next_node1).explored:
            greedy_score = graph.goto_index(start).greedy_score + graph.goto_index(next_node1).length
            self.add_heap_node(greedy_score, next_node1)

        if next_node2 is not None and not graph.goto_index(next_node2).explored:
            greedy_score = graph.goto_index(start).greedy_score + graph.goto_index(next_node2).length
            self.add_heap_node(greedy_score, next_node2)


def main():
    graph = Matrix()
    heap = Heap()

    with open("p081_matrix.txt", 'r') as f:
        for line in f:
            graph.add_row([Node(int(s), INF, False) for s in line.split(',')])

    # graph.view_matrix()

    target = (79, 79)
    print(dijkstra(graph, heap, target))
    print("Time elapsed:", time() - starting_time, "seconds")


def dijkstra(graph, heap, target, start=(0, 0)):
    graph.goto_index(start).explored = True
    graph.goto_index(start).greedy_score = graph.goto_index(start).length

    while start != target:
        heap.heapify(graph, start)

        heap_node = heap.extract_min()
        while graph.goto_index(heap_node.node_index).explored:
            heap_node = heap.extract_min()
        # heap.view_heap()

        start = heap_node.node_index
        graph.goto_index(start).greedy_score = heap_node.greedy_score
        graph.goto_index(start).explored = True

    return graph.goto_index(target).greedy_score


if __name__ == '__main__':
    main()
