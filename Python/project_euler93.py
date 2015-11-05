# Arithmetic expressions
"""
By using each of the digits from the set, {1, 2, 3, 4}, exactly once, 
and making use of the four arithmetic operations
(+, −, *, /) and brackets/parentheses, it is possible to 
form different positive integer targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3) − 1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one 
different target numbers of which 36 is the maximum,
and each of the numbers 1 to 28 can be obtained before 
encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which 
the longest set of consecutive positive integers, 1 to n,
can be obtained, giving your answer as a string: abcd.
"""

from time import time
from itertools import product
import operator


set_of_operators = {operator.add, operator.sub, operator.mul, operator.truediv}
rp_pattern_cache = {4: [['num', 'num', 'num', 'num',  'op',  'op',  'op'],
                        ['num', 'num', 'num',  'op', 'num',  'op',  'op'],
                        ['num', 'num', 'num',  'op',  'op', 'num',  'op'],
                        ['num', 'num',  'op', 'num', 'num',  'op',  'op'],
                        ['num', 'num',  'op', 'num',  'op', 'num',  'op']]
                    }


def count_continuance(combination):
    continuance = set()
    for target in solve24(combination):
        continuance.add(target)

    for i in range(1, 1000):
        if i not in continuance:
            return i


def solve24(numbers):
    for i in permutation_formatted(numbers):  # number_patterns
        for j in product(set_of_operators, repeat=len(numbers) - 1):  # operator_patterns
            for k in reverse_polish_pattern(len(numbers)):  # combination_patterns
                notation = reverse_polish_notation(i, j, k)
                yield reverse_polish_eval(notation)


def pretty_print(results, input_numbers):
        print('\n', '===', len(results), "solution(s) for", sorted(input_numbers), '===')
        for result in results:
            print(' '.join([str(num_or_op) if isinstance(num_or_op, int) else num_or_op.__name__ for num_or_op in result]))


def reverse_polish_eval(stack):
    assert len([i for i in stack if isinstance(i, int)]) * 2 - len(stack) - 1 == 0

    if operator.pow in stack:
        return NotImplemented

    result_stack = []
    for num_or_op in stack:
        if isinstance(num_or_op, int):
            result_stack.append(num_or_op)
        else:
            x = result_stack.pop()
            y = result_stack.pop()
            try:
                result_stack.append(num_or_op(y, x))
            except ZeroDivisionError:
                return ZeroDivisionError

    return result_stack[0]


def permutation(numbers):
    """
    input: a list of numbers, repeat allowed
    output: all possible permutations of the given list
    """
    if len(numbers) == 1:
        return [numbers]

    result = []
    for i in range(len(numbers)):
        result.extend([j + [numbers[i]] for j in permutation(numbers[:i] + numbers[i+1:])])

    return result


def permutation_formatted(numbers):
    result = permutation(numbers)
    return sorted(set([tuple(i) for i in result]))


def reverse_polish_pattern(input_count):
    """
    Matching pattern: the number of numbers is larger than the number of operators at each point

    All possible patterns for 4 numbers:
    num num num num  op  op  op
    num num num  op num  op  op
    num num num  op  op num  op
    num num  op num num  op  op
    num num  op num  op num  op
    """

    if input_count in rp_pattern_cache:
        return rp_pattern_cache[input_count]

    def rp_pattern(n, patterns):
        present_length = len(patterns)
        for i in range(present_length):
            if patterns[i].count('num') < n:
                if patterns[i].count('num') - patterns[i].count('op') > 1:
                    patterns.append(patterns[i].copy() + ['op'])
                    patterns[i].append('num')
                else:
                    patterns[i].append('num')
            else:
                patterns[i].append('op')

    result = [[]]
    while len(result[0]) < 2 * input_count - 1:
        rp_pattern(input_count, result)
    rp_pattern_cache[input_count] = result

    return result


def reverse_polish_notation(number_pattern, operator_pattern, combination_pattern):
    assert len(number_pattern) + len(operator_pattern) == len(combination_pattern)
    number_pattern_local, operator_pattern_local = list(number_pattern), list(operator_pattern)
    return [number_pattern_local.pop(0) if num_or_op == 'num' else operator_pattern_local.pop(0) for num_or_op in combination_pattern]


def main():
    max_index = []
    max_num = 0
    for i in range(1, 10):
        for j in range(i + 1, 10):
            for k in range(j + 1, 10):
                for l in range(k + 1, 10):
                    combination = [i, j, k, l]
                    this = count_continuance(combination)
                    if this > max_num:
                        max_index = combination
                        max_num = this
    print(max_num, max_index)


if __name__ == '__main__':
    starting_time = time()
    main()
    print('Total running time:', time() - starting_time, 'second(s)')
