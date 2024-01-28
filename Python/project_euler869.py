# build a prefix tree, starting from the least significant bit
#
# Let X be a random variable representing points that the optimal
# player will get.
# Let X_i be a random variable representing the points at ith bit.
# X_0 is for the least significant bit.
# E[X_i] = 1 * P(X_i=1) + 0 * P(X_i=0) = P(X_i=1)
# In other words, E[X_i] is equal to the chance of guessing ith bit correctly.
# We would always guess ith bit so that P(X_i=1) > 0.5
#
# X = X_0 + ... + X_k
# E[X] = E[X_0] + ... + E[X_k]
# However, it's hard to compute E[X_i] = P(X_i=1) directly.
#
# Instead compute conditional expectation based on the prefix path of the tree:
# E[X_i+...|prefix] = E[X_i|prefix] + E[X_{i+1}+...|prefix]
#                   = P(X_i=1|prefix) + E[X_{i+1}+...|prefix followed by 0] * P(followed by 0|prefix) + E[X_{i+1}+...|prefix followed by 1] * P(followed by 1|prefix)
# We can then recurse on E[X_{i+1}+...|prefix followed by 0 or 1]


from math import log2, ceil

from mathutil import prime_under


def tree_size(root):
    return root[2] if root is not None else 0


def contains_leaf(root):
    return tree_size(root[0]) + tree_size(root[1]) < tree_size(root)


def is_only_leaf(root):
    return root == [None, None, 1]


def insert(root, n):
    if root is None:
        root = [None, None, 0]
    root[2] += 1

    if n == 0:
        return root

    bit = n & 1
    root[bit] = insert(root[bit], n >> 1)

    return root


def build_tree(nums):
    root = [
        None,  # left child, 0
        None,  # right child, 1
        0,     # size of subtree from this node
    ]

    for n in nums:
        insert(root, n)
    return root


def expectation(root):
    if root is None:
        return 0

    size = tree_size(root)
    left_size = tree_size(root[0])
    right_size = tree_size(root[1])
    # probability of guessing next bit right: we will always guess the larger subtree
    prob = max(left_size, right_size) / size

    return prob + \
        expectation(root[0]) * left_size / size + \
        expectation(root[1]) * right_size / size


if __name__ == '__main__':
    import time
    start_t = time.time()

    primes = prime_under(10**8)
    print(f'Found {len(primes)} primes: {time.time()-start_t:.2f}s')

    tree = build_tree(primes)
    print(f'Built tree: {time.time()-start_t:.2f}s')

    print(expectation(tree))
    print(f'Found solution: {time.time()-start_t:.2f}s')
