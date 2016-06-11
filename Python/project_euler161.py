# project euler 161: Triominoes

"""
tile region with the following blocks

form1: XXX

form2: XX
       X
"""


# hard-coded state proprogation
class Tiling2:

    def __init__(self, n):
        if n % 3 != 0:
            return
        self.n = n
        self.A = [0] * (n + 1)
        self.B = [0] * (n + 1)
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


class Tiling3:

    def __init__(self, n):
        self.n = n
        self.A = [0] * (n + 1)
        self.B = [0] * (n + 1)
        self.A[0] = 1

    def solve(self):
        return self.A_sub(self.n)

    def A_sub(self, n):
        """
        ___
        ___
        ___

        __X _XX
        __X _X_
        __X ___

        _XX ___ __X ___
        __X __X _XX _XX
        ___ _XX ___ __X

        XXX     XXX
        ___ ==> XXX
        ___     XXX
        """
        if n < 0:
            return 0
        elif self.A[n] != 0:
            return self.A[n]
        else:
            self.A[n] = self.A_sub(n - 1) + self.A_sub(n - 2) + \
                2 * self.B_sub(n) + 2 * self.B_sub(n - 1) + \
                self.A_sub(n - 3)
            return self.A[n]

    def B_sub(self, n):
        """
        _XX
        __X
        ___
        """
        if n <= 0:
            return 0
        elif self.B[n] != 0:
            return self.B[n]
        else:
            self.B[n] = self.A_sub(n - 2) + self.A_sub(n - 3) + \
                self.B_sub(n - 3)
            return self.B[n]


class Tiling_n:
    """
    state at each stage is represented as n-element tuple
    """

    def __init__(self, n, m):
        """n by m grid
        propagate change along column
        """
        self.n = n
        self.state_cache = {}
        self.cache = {}

    def solve(self):
        """
        can only propagate in one direction, because of 1 to many mapping
        """

    def fill_col(self, state):
        if state in self.state_cache:
            return self.state_cache[state]

        after_states = []
        finished = False
        while not finished:
            aft_state = list[state]
            for index in range(self.n):
                if aft_state[index] > 0:  # 1, 2
                    continue
                # horizontal form1 tile
                aft_state[index] += 3

            after_states.append(tuple(map(lambda x: x - 1, aft_state)))

        self.state_cache[state] = after_states
        return after_states


def state_after_fill_col(index, state):
    if index >= len(state):
        return []

    if state[index] > 0:    # state[index] = 1 or 2
        return state_after_fill_col(index + 1, state)

    # state[index] = 0
    after_states = []

    if index <= len(state) - 1:
        aft_state = list(state)
        aft_state[index] += 3
        after_states.append(aft_state)
    elif index <= len(state) - 2:
        if state[index + 1] == 0:
            aft_state = list(state)

    return after_states


def main():
    print(Tiling2(9).solve())
    print(Tiling3(9).solve())

    print(Tiling2(3).solve())
    print(Tiling3(2).solve())


if __name__ == '__main__':
    main()
