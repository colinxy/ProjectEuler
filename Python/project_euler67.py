# Maximum path sum II
"""
By starting at the top of the triangle below and moving to 
adjacent numbers on the row below, the maximum total from
top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in triangle.txt 
(right click and 'Save Link/Target As...'), a 15K text file
containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. 
It is not possible to try every route to solve this problem,
as there are 2**99 altogether! If you could check 
one trillion routes every second it would take over twenty
billion years to check them all. There is an efficient algorithm 
to solve it. ;o)
"""


class Node(object):
    def __init__(self, node_weight, max_score):
        self.node_weight = node_weight
        self.max_score = max_score


class Matrix(object):
    def __init__(self):
        self.nodes = []

    def reset(self):
        for i in range(len(self.nodes)):
            for j in range(len(self.nodes[i])):
                self.goto_index((i, j)).max_score = 0

    def view_matrix(self):
        for i in self.nodes:
            for j in i:
                print(j.node_weight, end=' ')
            print('\n')

    def add_row(self, row):
        self.nodes.append(row)

    def goto_index(self, index):
        return self.nodes[index[0]][index[1]]

    def next_nodes(self, start):
        left_parent, right_parent = None, None
        if start == (0, 0):
            return left_parent, right_parent

        if 0 <= start[1] - 1:
            left_parent = (start[0] - 1, start[1] - 1)
        if start[1] < len(self.nodes[start[0] - 1]):
            right_parent = (start[0] - 1, start[1])

        return left_parent, right_parent


def max_path_sum(graph, start):
    if graph.goto_index(start).max_score != 0:
        return graph.goto_index(start).max_score

    left_parent, right_parent = graph.next_nodes(start)
    if left_parent is not None:
        left_sum = max_path_sum(graph, left_parent)
    else:
        left_sum = 0

    if right_parent is not None:
        right_sum = max_path_sum(graph, right_parent)
    else:
        right_sum = 0

    graph.goto_index(start).max_score = graph.goto_index(start).node_weight + max(left_sum, right_sum)
    return graph.goto_index(start).max_score


def main():
    graph = Matrix()

    with open("p067_triangle.txt", 'r') as f:
        for line in f:
            graph.add_row([Node(int(s), 0) for s in line.split(' ')])

    """graph.add_row([Node(3, 0)])
    graph.add_row([Node(7, 0), Node(4, 0)])
    graph.add_row([Node(2, 0), Node(4, 0), Node(6, 0)])
    graph.add_row([Node(8, 0), Node(5, 0), Node(9, 0), Node(3, 0)])"""

    # graph.view_matrix()

    maximum_path_sum = 0
    for current_column in range(100):
        curr = max_path_sum(graph, (len(graph.nodes)-1, current_column))
        if curr > maximum_path_sum:
            maximum_path_sum = curr

    print(maximum_path_sum)


if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("running time:", time()-starting_time, "seconds")
