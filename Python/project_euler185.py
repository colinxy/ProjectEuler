
class NumberMind:
    def __init__(self, guesses, correctness):
        self.guesses = []
        for line in guesses.splitlines():
            self.guesses.append(list(map(int, line)))
        self.correctness = correctness
        assert len(self.guesses) == len(self.correctness)
        self.trial = len(self.guesses)
        self.size = len(self.guesses[0])
        self.sequence = [-1] * self.size  # final answer
        self.check = [([False] * self.size) for _ in range(self.trial)]

    def solve(self):
        try:
            self._solve(0, 0)
        except Exception as e:
            print(e.args[0])
        return self.sequence

    def _solve(self, row, col):
        if self.sequence[col] == self.guesses[row][col]:
            self.check[row][col] = True
            if self.check[row][:col+1].count(True) > self.correctness[row]:
                self.check[row][col] = False
                return
            if col == self.size-1:
                if self.check[row].count(True) != self.correctness[row]:
                    return
                elif row == self.trial-1:
                    print(self.sequence)
                    raise Exception("solution found")
                self._solve(row+1, 0)
            else:
                self._solve(row, col+1)
            self.sequence[col] = -1
        else:
            # has to be False
            if self.sequence[col] == -1:
                for the_row in self.guesses[:row]:
                    if self.guesses[row][col] == the_row[col]:
                        break
                else:

                    # possibly True
                    if self.sequence[col] == -1 and self.check[row][:col].count(True) < self.correctness[row]:
                        self.check[row][col] = True
                        self.sequence[col] = self.guesses[row][col]
                        if col == self.size-1:
                            if self.check[row].count(True) != self.correctness[row]:
                                self.check[row][col] = False
                                self.sequence[col] = -1
                                return
                            elif row == self.trial-1:
                                print(self.sequence)
                                raise Exception("solution found")
                            self._solve(row+1, 0)
                        else:
                            self._solve(row, col+1)
                        self.sequence[col] = -1

            # False
            self.check[row][col] = False
            if col == self.size-1:
                if self.check[row].count(True) != self.correctness[row]:
                    return
                elif row == self.trial-1:
                    print(self.sequence)
                    raise Exception("solution found")
                self._solve(row+1, 0)
            else:
                self._solve(row, col+1)

    def __str__(self):
        return '\n'.join(str(i)+'  '+str(j) for i, j in zip(self.guesses, self.correctness))


def main():
    guesses_tiny = (
        "90342\n"
        "70794\n"
        "39458\n"
        "34109\n"
        "51545\n"
        "12531\n"
    )
    correctness_tiny = [2, 0, 2, 1, 2, 1]
    guesses = ("5616185650518293\n"
               "3847439647293047\n"
               "5855462940810587\n"
               "9742855507068353\n"
               "4296849643607543\n"
               "3174248439465858\n"
               "4513559094146117\n"
               "7890971548908067\n"
               "8157356344118483\n"
               "2615250744386899\n"
               "8690095851526254\n"
               "6375711915077050\n"
               "6913859173121360\n"
               "6442889055042768\n"
               "2321386104303845\n"
               "2326509471271448\n"
               "5251583379644322\n"
               "1748270476758276\n"
               "4895722652190306\n"
               "3041631117224635\n"
               "1841236454324589\n"
               "2659862637316867\n"
               )
    correctness = [2, 1, 3, 3, 3, 1, 2, 3, 1, 2, 3, 1, 1, 2, 0, 2, 2, 3, 1, 3, 3, 2]
    print(''.join(map(str, NumberMind(guesses_tiny, correctness_tiny).solve())))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # infeasible for large problems
    print("Time elapsed:", time() - starting_time, "seconds")
