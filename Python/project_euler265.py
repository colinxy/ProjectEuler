
from functools import reduce


def check(n, arrangement):
    subseqs = set()
    for i in range(2**n-n+1):
        subseqs.add(tuple(arrangement[i:i+n]))
    for i in range(2**n-n+1, 2**n):
        subseqs.add(tuple(arrangement[i:] + arrangement[:i+n-2**n]))
    return len(subseqs) == 2**n


def bin_circle(n):
    def dfs(idx, arrangement, subseqs):
        if idx == 2**n:
            # or more simplistic approach: check equal number of 0s and 1s
            if check(n, arrangement):
                solutions.append(list(arrangement))
            return

        for i in (0, 1):
            arrangement[idx] = i
            subseq = tuple(arrangement[idx+1-n:idx+1])
            if subseq in subseqs:
                continue
            subseqs.add(subseq)
            dfs(idx+1, arrangement, subseqs)
            subseqs.remove(subseq)

    solutions = []
    dfs(n, [0] * (2**n), {(0, ) * n})
    return solutions


def bit_reverse(num, bits):
    for i in range(bits//2):
        lsb = (num & (1 << i)) >> i
        msb = (num & (1 << (bits-1-i))) >> (bits-1-i)
        num ^= (msb ^ lsb) << i
        num ^= (msb ^ lsb) << (bits-1-i)
    return num


def bin_circle_bit(n):
    """same algorithm, use bit level trick"""
    def dfs(idx, arrangement, subseqs):
        """
        arrangement: (2**n) bits integer
        subseqs:     (2**n) bits integer, each bit is a subseq
        """
        if idx == 2**n:
            # same number of 0s and 1s
            if "{:0{width}b}".format(arrangement,
                                     width=2**n).count('0') == 2**(n-1):
                solutions.append(arrangement)
            return

        for bit in (0, 1):
            # bit set at idx
            arrangement = (arrangement & (~(1 << idx))) | (bit << idx)
            # print("{:0{width}b}".format(arrangement, width=2**n))
            # bit record
            record = ((arrangement
                       & (((1 << (idx+1))-1) - ((1 << (idx+1-n))-1)))
                      >> (idx-n))
            # print("{:0{width}b} {}".format(record, idx, width=2**n))
            subseq = 1 << record
            if subseqs & subseq:
                continue
            # bit set, bit 0 => 1
            subseqs ^= subseq
            # print("{:0{width}b}".format(subseqs, width=2**n))
            dfs(idx+1, arrangement, subseqs)
            # bit unset, bit 1 => 0
            subseqs ^= subseq

    solutions = []
    dfs(n, 0, 1)
    # solutions bits are in reverse
    # print("{:0{width}b}".format(solutions[0], width=2**n))
    return [bit_reverse(sol, 2**n) for sol in solutions]


def main():
    N = 5

    # normal approach
    # solutions = bin_circle(N)
    # print(len(solutions))
    # print(sum(reduce(lambda acc, x: acc*2+x, sol) for sol in solutions))

    # use bit trick, runs in half amount of time
    solutions = bin_circle_bit(N)
    # print(len(solutions))
    print(sum(solutions))


if __name__ == '__main__':
    main()
