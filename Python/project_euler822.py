import heapq
from math import ceil, log2

# MOD is prime
MOD = 1234567891


def S_naive(n, m):
    heap = [(i, i) for i in range(2, n+1)]
    heapq.heapify(heap)
    for _ in range(m):
        sq_n, n = heapq.heappop(heap)
        heapq.heappush(heap, (sq_n*sq_n, n))

    # nums = sorted(heap, key=lambda v: v[1])
    # print([sq_n for sq_n, n in nums])
    return sum(sq_n for sq_n, _ in heap) % MOD


def sq_mod_basic(n, exp):
    # pow(n, 2**exp, MOD) is faster due to C implementation
    for _ in range(exp):
        n = n * n % MOD
    return n

def sq_mod_empirical(n, exp):
    """
    empirically, repeated square mod appears to cycle every 13710012.
    (may not be the smallest cycle)
    This number is related to MOD-1 = 1234567890

    XXX: prove this
    """
    MAGIC = 13710012
    for _ in range(exp % MAGIC):
        n = n * n % MOD
    return n

def sq_mod_cycle(n, exp):
    """
    Detect cycles in repeated squaring

    XXX: this algorithm is very memory hungry and slow!
    Consider using "tortoise and hare" algorithm
    https://en.wikipedia.org/wiki/Cycle_detection
    """
    trail = [n]
    trail_map = {n: 0}          # mapping to index in trail
    for i in range(1, exp+1):
        n = n * n % MOD

        if n in trail_map:
            steps2cycle = trail_map[n]
            cycle = trail[steps2cycle:]
            print(trail[0], steps2cycle, len(cycle))

            cycle_idx = (exp - steps2cycle) % len(cycle)
            return cycle[cycle_idx]

        trail.append(n)
        trail_map[n] = i

    return n


# NOTE: ignore the above inefficient attempts
def sq_mod(n, exp):
    """
    Solve n**(2**exp) (mod MOD)
    """
    # Fermat's Little Theorem, given MOD is prime
    # n**(MOD-1) === 1 (mod MOD)
    exp_mod = pow(2, exp, MOD-1)
    return pow(n, exp_mod, MOD)


def S_heapq(nums_log, nums_sq_count, total_rounds):
    heap = [(l, i) for i, l in enumerate(nums_log)]
    heapq.heapify(heap)

    for _i in range(total_rounds):
        min_l, min_i = heapq.heappop(heap)
        nums_sq_count[min_i] += 1
        # min_l = log2(n) => 2*min_l = log2(n^2)
        heapq.heappush(heap, (min_l*2, min_i))


def S_reduce(nums_log, nums_sq_count, idx0, rounds0):
    """
    Have the smallest number "n0" go through `rounds0` of squaring,
    and squaring other numbers accordingly so that "n0" remains smallest

    idx0: the index of the smallest number
    rounds0: the number of times that the smallest number will be squared

    Returns the total number of iterations by all numbers.
    """
    # nums_log[idx0] <=> log2(log2(n0)) + taken rounds
    nums_sq_count[idx0] += rounds0
    nums_log[idx0] += rounds0

    total_rounds = rounds0
    for i in range(len(nums_log)):
        if i == idx0:
            continue

        rounds_i = ceil(nums_log[idx0] - nums_log[i])
        nums_log[i] += rounds_i
        nums_sq_count[i] += rounds_i

        total_rounds += rounds_i

    return total_rounds


def S(n, m):
    # take log2 twice
    # n^(2^k) => log2(n) * 2^k => log2(log2(n)) + k
    nums_log = [log2(log2(i)) for i in range(2, n+1)]
    nums_sq_count = [0] * (n-1)

    rounds_left = m
    print('Rounds left: {}'.format(rounds_left))
    while rounds_left >= n-1:
        # square rounds_left//(n-1) rounds to the smallest number,
        # note that this won't exceed rounds_left because bigger numbers will use fewer rounds
        rounds = S_reduce(nums_log, nums_sq_count, 0, rounds_left//(n-1))
        rounds_left -= rounds
        print('Rounds left: {}'.format(rounds_left))

    S_heapq(nums_log, nums_sq_count, rounds_left)
    print('Rounds left: 0')

    # print(nums_sq_count)
    # inspecting nums_sq_count, we can find they are very close to each other.

    nums_mod = [sq_mod(i, nums_sq_count[i-2]) for i in range(2, n+1)]
    return sum(nums_mod) % MOD


if __name__ == '__main__':
    print(S(10**4, 10**16))
