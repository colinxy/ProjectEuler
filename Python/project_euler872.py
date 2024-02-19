# First few trees T_n
#
# T1:
# 1
#
# T2:
#   2
# 1
#
# T3:
#   3
# 2   1
#
# T4:
#    4
#  3   2
# 1
#
# T5:
#    5
#  4 3 1
# 2
#
# T6:
#     6
#  5  4  2
# 3 1
#
# T7:
#       7                     0
#  6    5    3           1    2    4
# 4 2  1                2 4  4
#
# T8:
#        8                    0
#   7    6    4          1    2    4
#  5 3  2               2 4  4
# 1                    4
#
#
# Rewrite the tree in terms of the difference between each node and
# its parent.  We can observe that:
# 1. For each parent, its direct children are consecutive powers of 2,
# starting from twice the parent.
# 2. Each transformation (T_{n-1} => T_n) takes the left most path
# (also longest) and makes them the direct children of the new root.
# These new children's differences from the new root are also
# consecutive powers of 2, starting from 1.
#
# Above observations can be proved by induction.
#
# Going back to the original tree: think of the tree as a binary
# suffix tree of the difference between root and each node.  The path
# between root (n) and any node (k) can be interpreted as the binary
# suffix of their difference (n-k)


def f(n, k):
    diff = n - k

    node = n
    path = node

    bit = 1
    while diff >= bit:
        if diff & bit:
            node -= bit
            path += node
            # print(node)

        bit <<= 1

    return path


if __name__ == '__main__':
    print(f(6, 1))
    print(f(10, 3))
    print(f(10**17, 9**17))
