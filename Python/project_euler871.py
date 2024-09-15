# Observation:
# for every i leads to a cycle after repeatedly applying f;
# each node from a cycle is the root of the tree of connected nodes;
# once we have a tree, we can apply greedy algorithm that picks edges closer to the leaf first.

from collections import defaultdict


def f(n, i):
    return (i*i*i + i + 1) % n


def D_brute(n, A2, start_i=0):
    """
    A2: A union f(A)
    """
    if start_i == n:
        return 0

    max_subset = 0

    for i in range(start_i, n):
        f_i = f(n, i)
        if i == f_i or i in A2 or f_i in A2:
            continue

        A2.add(i)
        A2.add(f_i)

        max_subset = max(max_subset, D_brute(n, A2, i+1) + 1)

        A2.remove(f_i)
        A2.remove(i)

    return max_subset


def f_rev(n):
    """
    Reversed directed graph of f_n, mapping from target node to source nodes
    """
    freq = defaultdict(set)
    for i in range(n):
        v = f(n, i)
        freq[v].add(i)

    return freq


def find_cycle(n, i):
    # find the cycle that i leads to
    path = [i]
    path_set = {i}

    curr = i
    while True:
        curr = f(n, curr)
        if curr in path_set:
            idx = path.index(curr)
            return path[:idx], path[idx:]

        path.append(curr)
        path_set.add(curr)


def visit_tree(graph_rev, root, visited):
    """Visit a tree from node `root`.
    Returns: Tuple(
        int: the max number of edges with no overlapping nodes
        bool: is the root node leftover (not used in above edges)?
    )

    Assumes no cycle from node `root`.

    Observation: each node on the cycle is the root of a tree in the reversed graph, excluding cycle nodes
    """
    if root in visited:
        raise Exception(f"BUG: should not revisit {root}!")
    visited.add(root)

    total_edges = 0
    leftover = True

    for child in graph_rev[root]:
        child_total_edges, child_leftover = visit_tree(graph_rev, child, visited)
        total_edges += child_total_edges
        if leftover and child_leftover:
            total_edges += 1
            leftover = False

    return total_edges, leftover


def leftover_cycle(leftovers):
    """Count the number of edges that can be formed from leftovers on a cycle.
    That is, find the max number of pairs of consecutive Trues (non overlapping).

    # with wrap around (cycle)
    >>> leftover_cycle([True, False, False, True, True, False, True, True, True])
    3
    # without wrap around
    >>> leftover_cycle([True, False, False, True, True, False, True, True, True, False])
    2
    >>> leftover_cycle([False, True, False, True, True, False, True, True, True])
    2
    """
    runs = []
    run_curr = 0
    for i in range(len(leftovers)):
        if leftovers[i]:
            run_curr += 1
        elif run_curr != 0:
            runs.append(run_curr)
            run_curr = 0
    if run_curr != 0:
        runs.append(run_curr)

    # wrap around
    if len(runs) > 1 and leftovers[0] and leftovers[-1]:
        runs[0] += runs[-1]
        runs.pop()

    return sum(r//2 for r in runs)


def D(n):
    graph_rev = f_rev(n)

    visited = set()
    total_edges = 0

    for i in range(n):
        if i in visited:
            continue

        _pre_cycle, cycle = find_cycle(n, i)

        cycle_total_edges = 0
        leftovers = []
        for i in range(len(cycle)):
            # remove the next element in the cycle to avoid looping
            # (previous element in the reverse graph)
            next_in_cycle = cycle[(i-1) % len(cycle)]
            graph_rev[cycle[i]].remove(next_in_cycle)

            root_total_edges, root_leftover = visit_tree(graph_rev, cycle[i], visited)
            cycle_total_edges += root_total_edges
            leftovers.append(root_leftover)

        cycle_total_edges += leftover_cycle(leftovers)
        # print(n, len(cycle), cycle_total_edges)

        total_edges += cycle_total_edges

    return total_edges


if __name__ == '__main__':
    total = 0
    for i in range(10**5+1, 10**5+101):
        d = D(i)
        total += d

    print(total)
