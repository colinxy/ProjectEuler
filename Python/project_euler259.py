# Problem 259: Reachable Numbers

from __future__ import print_function
from __future__ import division

import operator
from fractions import Fraction
from functools import reduce
from itertools import product

OPS = [operator.add, operator.sub, operator.mul, operator.truediv]


def eval(op_stack):             # eval postfix expr
    op_stack = op_stack[::-1]
    while len(op_stack) > 1:
        num1 = op_stack.pop()
        num2 = op_stack.pop()
        op = op_stack.pop()
        op_stack.append(op(num1, num2))  # could raise ZeroDivisionError
    return op_stack.pop()


def split(n, numbers=[1, 2, 3, 4, 5, 6, 7, 8, 9]):  # do not mutate `numbers`
    if n == 0:
        return [[numbers]]
    if n >= len(numbers):
        return []

    results = []
    for i in range(1, len(numbers)):
        results.extend(map(lambda l: [numbers[:i]] + l,
                           split(n-1, numbers[i:])))
    return results


def split1_9():
    results = []
    for nspl in range(9):
        results.extend([reduce(lambda acc, x: acc*10+x, s) for s in spl]
                       for spl in split(nspl))
    return results


def eval_numbers(nums):
    cache = {}

    def eval_nums(beg, end):
        if (beg, end) in cache:
            return cache[(beg, end)]

        res = set()
        if beg + 1 == end:
            res.add(nums[beg])
        for mid in range(beg+1, end):
            for left_val in eval_nums(beg, mid):
                for right_val in eval_nums(mid, end):
                    res.add(left_val + right_val)
                    res.add(left_val - right_val)
                    res.add(left_val * right_val)
                    if right_val != 0:
                        res.add(Fraction(left_val, right_val))

        cache[(beg, end)] = res
        return res

    for beg in range(len(nums)):
        for end in range(beg+1, len(nums)+1):
            cache[(beg, end)] = eval_nums(beg, end)

    return cache.get((0, len(nums)), set())


def all_eval(nums, ops):        # produce all patterns of nums and ops
    results = []

    def eval_from(num_idx, op_idx, stack):
        if len(nums) == num_idx and len(ops) == op_idx:
            results.append(stack[0])
            return

        if num_idx < len(nums):
            stack.append(nums[num_idx])
            eval_from(num_idx+1, op_idx, stack)
            stack.pop()

        if len(stack) > 1:
            num1 = stack.pop()
            num2 = stack.pop()
            try:
                stack.append(ops[op_idx](num2, num1))  # operand reverse order
                eval_from(num_idx, op_idx+1, stack)
                stack.pop()
            except ZeroDivisionError:
                pass
            stack.append(num2)
            stack.append(num1)

    eval_from(0, 0, [])
    return results


def reachable_pos_ints():
    # with pypy, 5mins
    # for seq in split1_9():
    #     seq = [Fraction(i) for i in seq]
    #     for ops in product(OPS, repeat=len(seq)-1):
    #         # print(seq, ops)
    #         for res in all_eval(seq, ops):
    #             if res.denominator == 1 and res.numerator > 0:
    #                 # print(res.numerator)
    #                 yield res.numerator

    # with pypy, 1min
    for seq in split1_9():
        for res in eval_numbers(seq):
            if res < 0:
                continue
            if isinstance(res, int):
                yield res
            elif isinstance(res, Fraction) and res.denominator == 1:
                yield res.numerator


def main():
    # all_splits = split1_9()
    # print(len(all_splits))
    # for s in all_splits:
    #     print(s)

    reachables = set(reachable_pos_ints())
    # print(pos_reachables)
    # print(len(reachables))
    print(sum(reachables))


if __name__ == '__main__':
    main()


# My improved solution is based on the dp solution below.
# While still not as good as the solution below, it uses the dp idea on
# each splitted group of numbers

"""
# excellent dp solution by tolstopuz

import fractions

def gen(m, n):
    x = m
    r = set()
    for i in range(m + 1, n):
        for a in cache[(m, i)]:
            for b in cache[(i, n)]:
                r.add(a + b)
                r.add(a - b)
                r.add(a * b)
                if b:
                    if isinstance(a, int) and isinstance(b, int) and a % b == 0:
                        r.add(a // b)
                    else:
                        r.add(fractions.Fraction(a, b))
        x = x * 10 + i
    r.add(x)
    return r

nmax = 10

cache = {}
for l in range(1, nmax + 1):
    for i in range(1, nmax - l + 1):
        cache[(i, i + l)] = gen(i, i + l)

s = sum(x for x in cache[(1, nmax)]
        if x > 0 and (isinstance(x, int) or x.denominator == 1))
print(s)
"""
