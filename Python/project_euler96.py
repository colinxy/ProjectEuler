# Su Doku
"""
Su Doku (Japanese meaning number place) is the name given to
a popular puzzle concept. Its origin is unclear, but credit
must be attributed to Leonhard Euler who invented a similar,
and much more difficult, puzzle idea called Latin Squares.
The objective of Su Doku puzzles, however, is to replace the
blanks (or zeros) in a 9 by 9 grid in such that each row,
column, and 3 by 3 box contains each of the digits 1 to 9.
Below is an example of a typical starting puzzle grid and
its solution grid.

A well constructed Su Doku puzzle has a unique solution and
can be solved by logic, although it may be necessary to employ
"guess and test" methods in order to eliminate options (there
is much contested opinion over this). The complexity of the
search determines the difficulty of the puzzle; the example
above is considered easy because it can be solved by straight
forward direct deduction.

The 6K text file, sudoku.txt (right click and 'Save Link/Target As...'),
contains fifty different Su Doku puzzles ranging in difficulty,
but all with unique solutions (the first puzzle in the file is the example above).

By solving all fifty puzzles find the sum of the 3-digit numbers
found in the top left corner of each solution grid; for example,
483 is the 3-digit number found in the top left corner of the
solution grid above.
"""

import numpy as np


class Sudoku(object):
    def __init__(self, str_sudoku=None):
        self.sudoku = np.zeros((9, 9), dtype=np.uint8)
        self.certain = np.zeros((9, 9), dtype=np.bool)
        if str_sudoku is not None:
            for row, the_row in enumerate(str_sudoku.splitlines()):
                for col, num in enumerate(the_row):
                    self.sudoku[row, col] = np.uint8(num)
                    if self.sudoku[row, col] != 0:
                        self.certain[row, col] = True

    def get_all_small_grid(self):
        return np.array([self.sudoku[3*rows:3*rows+3, 3*cols:3*cols+3] for rows in range(3) for cols in range(3)])

    def get_the_small_grid(self, row, col):
        rows, cols = row // 3, col // 3
        return self.sudoku[3*rows:3*rows+3, 3*cols:3*cols+3]

    def get_row(self, row):
        return self.sudoku[row, :]

    def get_col(self, col):
        return self.sudoku[:, col]

    def check(self):
        for i in range(9):
            if not set(self.sudoku[i,:]) == set(range(1, 10)):
                return False
        for i in range(9):
            if not set(self.sudoku[:,i]) == set(range(1, 10)):
                return False
        for i in range(3):
            for j in range(3):
                if not set(self.sudoku[3*i:3*i+3, 3*j:3*j+3].flat) == set(range(1, 10)):
                    return False
        return True

    def simplify(self):

        changed = True
        while changed:
            changed = False
            for row in range(9):
                for col in range(9):
                    if self.sudoku[row, col] == 0:
                        used_numbers = set(self.sudoku[row,:]) | set(self.sudoku[:,col]) | set(self.get_the_small_grid(row, col).flat)
                        candidates = set(range(1, 10)) - used_numbers
                        if len(candidates) == 1:
                            self.sudoku[row, col] = candidates.pop()
                            self.certain[row, col] = True
                            changed = True

            for row in [0, 3, 6]:
                for col in [0, 3, 6]:
                    candidates = set(range(1, 10)) - set(self.get_the_small_grid(row, col).flat)
                    spots = np.ones((10, 3, 3), dtype=np.bool)
                    # preprocess spots, marking possible place to put the number
                    for row_ in range(3):
                        for col_ in range(3):
                            if self.sudoku[row+row_, col+col_] != 0:
                                spots[:, row_, col_].fill(False)
                    for row_ in range(3):
                        for i in self.sudoku[row+row_, :]:
                            if i != 0:
                                spots[i, row_, :].fill(False)
                    for col_ in range(3):
                        for i in self.sudoku[:, col+col_]:
                            if i != 0:
                                spots[i, :, col_].fill(False)

                    for num in candidates:
                        if list(spots[num, :, :].flat).count(True) == 1:
                            index = list(spots[num, :, :].flat).index(True)
                            row_, col_ = divmod(index, 3)
                            self.sudoku[row+row_, col+col_] = num
                            self.certain[row+row_, col+col_] = True
                            changed = True

    def conflict(self, row, col, val):
        if val in self.get_row(row) or val in self.get_col(col) or val in self.get_the_small_grid(row, col):
            return True
        return False

    def solve(self):
        if not self.certain[0, 0]:
            self._solve(0, 0)
        else:
            next_row_col = self._from2next(0, 0)
            if next_row_col is not None:
                self._solve(next_row_col[0], next_row_col[1])

    def _solve(self, row, col):
        for num in range(1, 10):
            if not self.conflict(row, col, num):
                self.sudoku[row, col] = num
                next_row_col = self._from2next(row, col)
                if next_row_col is None:
                    self.certain.fill(True)
                    return

                self._solve(next_row_col[0], next_row_col[1])

                if not self.certain[row, col]:
                    self.sudoku[row, col] = 0

    def _from2next(self, row, col):  # return (row, col) value must be not certain
        while True:
            if (row, col) == (8, 8):
                return None
            col += 1
            if col == 9:
                row, col = row+1, 0
            if not self.certain[row, col]:
                return row, col

    # todo: incomplete, did not deal with rotation, etc
    def __eq__(self, other):
        return (self.sudoku == other.sudoku).all()

    def __str__(self):
        representation = "\n"
        for i in range(3):
            line = [str(j) for j in self.sudoku[i]]
            representation += ' '.join(line[:3]) + ' | ' + ' '.join(line[3:6]) + ' | ' + ' '.join(line[6:9]) + '\n'
        representation += '-' * 6 + '+' + '-' * 7 + '+' + '-' * 6 + '\n'
        for i in range(3, 6):
            line = [str(j) for j in self.sudoku[i]]
            representation += ' '.join(line[:3]) + ' | ' + ' '.join(line[3:6]) + ' | ' + ' '.join(line[6:9]) + '\n'
        representation += '-' * 6 + '+' + '-' * 7 + '+' + '-' * 6 + '\n'
        for i in range(6, 9):
            line = [str(j) for j in self.sudoku[i]]
            representation += ' '.join(line[:3]) + ' | ' + ' '.join(line[3:6]) + ' | ' + ' '.join(line[6:9]) + '\n'
        return representation

    def __repr__(self):
        return "Sudoku(" + '\n'.join(''.join(map(str, list(row))) for row in self.sudoku) + ')'


def main():
    answer = 0

    sudokus = open("p096_sudoku.txt").readlines()
    # solution_f = open("p096_sudoku_solution.txt", 'w')
    for i in range(len(sudokus) // 10):
        sudoku_str = ''.join(sudokus[i*10+1:i*10+10])
        sudoku = Sudoku(sudoku_str)
        # print(i+1)
        # print(sudoku)
        sudoku.simplify()
        sudoku.solve()
        # print(sudoku)
        if not sudoku.check(): print(i+1, sudoku)
        # solution_f.write("Grid {0:02d}\n".format(i+1))
        # solution_f.write(repr(sudoku).lstrip("Sudoku(").rstrip(")") + '\n')

        answer += int(''.join(map(str, sudoku.sudoku[0,:3])))

    # solution_f.close()

    print(answer)

    """
    a_game = Sudoku("003020600\n900305001\n001806400\n008102900\n700000008\n006708200\n002609500\n800203009\n005010300")
    print(a_game)
    a_game.solve()
    print(a_game)
    print(a_game.check())

    solution = Sudoku("483921657\n" +
                      "967345821\n" +
                      "251876493\n" +
                      "548132976\n" +
                      "729564138\n" +
                      "136798245\n" +
                      "372689514\n" +
                      "814253769\n" +
                      "695417382")
    print(solution)
    print(solution.check())
    print("valid_game == a_game:", solution == a_game)
    """

    """
    my_sudoku = Sudoku("000000000\n"
                       "600258001\n"
                       "020419030\n"
                       "500040009\n"
                       "030000070\n"
                       "900020003\n"
                       "010736090\n"
                       "300184002\n"
                       "000000000")
    print(my_sudoku)
    my_sudoku.simplify()
    print(my_sudoku)
    my_sudoku.solve()
    print(my_sudoku)
    """


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
