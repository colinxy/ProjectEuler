# project euler 161: Triominoes

"""
tile region with the following blocks

type A
XXX

type B
XX
X

idea: propagate along columns

0123456
 ------>
 ------>
 ------>
"""

from collections import defaultdict


# hard-coded state proprogation, not used in solving the real size problem
class Tiling2:

    def __init__(self, n):
        if n % 3 != 0:
            raise ArithmeticError("No Solution")
        self.n = n
        self.A = [0] * (n + 1)  # after putting a type A block
        self.B = [0] * (n + 1)  # after putting a type B block
        self.A[0] = 1

    def solve(self):
        return self.A_sub(self.n)

    def A_sub(self, n):
        if n <= 0:
            return self.A[0]
        elif self.A[n] != 0:
            return self.A[n]
        else:
            self.A[n] = self.A_sub(n - 3) + 2 * self.B_sub(n)
            return self.A[n]

    def B_sub(self, n):
        if n <= 0:
            return 0
        elif self.B[n] != 0:
            return self.B[n]
        else:
            self.B[n] = self.A_sub(n - 3) + self.B_sub(n - 3)
            return self.B[n]


def fill_col(state):
    if all(i > 0 for i in state):
        return [state]

    index = state.index(0)
    after_states = []

    # try:
    #     index = state.index(0)
    # except ValueError:
    #     return [state]

    # fill with type A
    # XXX
    after = list(state)
    after[index] = 3
    after_states.extend(fill_col(tuple(after)))
    # X
    # X
    # X
    if index < len(state)-2 and state[index+1] == 0 and state[index+2] == 0:
        after = list(state)
        after[index] = 1
        after[index+1] = 1
        after[index+2] = 1
        after_states.extend(fill_col(tuple(after)))

    # fill with type B, 1 to the left, 2 orientations
    # orient up
    #  X
    # XX
    if index > 0 and state[index-1] == 1:
        after = list(state)
        after[index] = 2
        after[index-1] = 2
        after_states.extend(fill_col(tuple(after)))

    # orient down
    # XX
    #  X
    if index < len(state)-1 and state[index+1] == 1:
        after = list(state)
        after[index] = 2
        after[index+1] = 2
        after_states.extend(fill_col(tuple(after)))
    if index < len(state)-2 and state[index+1] == 0 and state[index+2] == 0:
        # XX
        # XX
        # XX
        after = list(state)
        after[index] = 2
        after[index+1] = 2
        after[index+2] = 2
        after_states.extend(fill_col(tuple(after)))
    if index < len(state)-3 and state[index+1] == 0 and \
            state[index+2] == 0 and state[index+3] == 0:
        # XX
        # XX
        # X
        # X
        after = list(state)
        after[index] = 2
        after[index+1] = 2
        after[index+2] = 1
        after[index+3] = 1
        after_states.extend(fill_col(tuple(after)))

    # fill with type B, 2 to the left, 2 orientations
    # orient up/down
    if index < len(state)-1 and state[index+1] == 0:
        # XX
        # X
        after = list(state)
        after[index] = 2
        after[index+1] = 1
        after_states.extend(fill_col(tuple(after)))
        # X
        # XX
        after[index] = 1
        after[index+1] = 2
        after_states.extend(fill_col(tuple(after)))

    return after_states


class Tiling_n:
    def __init__(self, n, m):
        """n by m grid
        propagate change along columns, in a increasing manner

        each state is represented as a column, of size self.rows
        """
        self.rows = n
        self.cols = m
        self.state_cache = {}                   # state -> state
        self.curr_col_cache = defaultdict(int)  # state -> count

    def solve(self):
        empty_state = (0, ) * self.rows
        self.curr_col_cache[empty_state] = 1

        for i in range(self.cols):
            self._solve_col()
            # print(self.curr_col_cache)
            # print(self.state_cache)
        return self.curr_col_cache[empty_state]

    def _solve_col(self):
        next_col_cache = defaultdict(int)

        for state in self.curr_col_cache:
            if all(i > 0 for i in state):
                next_col_cache[tuple(i-1 for i in state)] += \
                    self.curr_col_cache[state]
            else:
                for aft_state in self.fill_column(state):
                    next_col_cache[tuple(i-1 for i in aft_state)] += \
                        self.curr_col_cache[state]

        self.curr_col_cache = next_col_cache

    def fill_column(self, state):
        if state in self.state_cache:
            return self.state_cache[state]

        after_states = fill_col(state)
        self.state_cache[state] = after_states
        return after_states


def main():
    print(Tiling2(9).solve())
    print(Tiling2(3).solve())

    print(Tiling_n(9, 12).solve())


if __name__ == '__main__':
    main()
