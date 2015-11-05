# Minimal network
"""
The following undirected network consists of seven vertices 
and twelve edges with a total weight of 243.


The same network can be represented by the matrix below.

    	A	B	C	D	E	F	G
A	-	16	12	21	-	-	-
B	16	-	-	17	20	-	-
C	12	-	-	28	-	31	-
D	21	17	28	-	18	19	23
E	-	20	-	18	-	-	11
F	-	-	31	19	-	-	27
G	-	-	-	23	11	27	-
However, it is possible to optimise the network by removing 
some edges and still ensure that all points on the network 
remain connected. The network which achieves the maximum 
saving is shown below. It has a weight of 93, representing 
a saving of 243 − 93 = 150 from the original network.


Using network.txt (right click and 'Save Link/Target As...'), 
a 6K text file containing a network with forty vertices, 
and given in matrix form, find the maximum saving which 
can be achieved by removing redundant edges whilst ensuring 
that the network remains connected.
"""


import heapq


def main():
    graph = []
    in_the_tree = []
    edges = []
    total = 0
    for source, line in enumerate(open("p107_network.txt").read().splitlines()):
        in_the_tree.append(False)
        vertex = []
        for target, edge in enumerate(line.split(",")):
            if edge == '-':
                continue
            edge_len = int(edge)
            vertex.append((edge_len, source, target))

            if source < target:
                edges.append((edge_len, source, target))
                total += edge_len
        graph.append(vertex)

    edges.sort(key=lambda d: d[0])  # key can actually be omitted

    edge_len, source, target = edges[0][0], edges[0][1], edges[0][2]  # the shortest edge must be in the tree
    min_span_tree = edge_len
    in_the_tree[source], in_the_tree[target] = True, True

    frontier = graph[source] + graph[target]
    heapq.heapify(frontier)

    while frontier:  # len(frontier) > 0
        candidate = heapq.heappop(frontier)
        edge_len, source, target = candidate[0], candidate[1], candidate[2]
        if in_the_tree[source] and in_the_tree[target]:
            continue
        min_span_tree += edge_len

        # at least one must be not in the tree
        if not in_the_tree[source]:
            in_the_tree[source] = True
            for future_candidate in graph[source]: 
                heapq.heappush(frontier, future_candidate)
        if not in_the_tree[target]:
            in_the_tree[target] = True
            for future_candidate in graph[target]: 
                heapq.heappush(frontier, future_candidate)

    print(total)
    print(min_span_tree)
    print(total - min_span_tree)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
