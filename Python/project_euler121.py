# Disc game prize fund
"""
A bag contains one red disc and one blue disc. In a game of chance a player takes a disc at random and
its colour is noted. After each turn the disc is returned to the bag, an extra red disc is added, and
another disc is taken at random.

The player pays £1 to play and wins if they have taken more blue discs than red discs at the end of the game.

If the game is played for four turns, the probability of a player winning is exactly 11/120, and so the
maximum prize fund the banker should allocate for winning in this game would be £10 before they would
expect to incur a loss. Note that any payout will be a whole number of pounds and also includes the
original £1 paid to play the game, so in the example given the player actually wins £9.

Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played.
"""

from functools import reduce
from operator import mul


def take_disc(chance, win_score, score=0):
    """
    :param chance: the chance of player to score red in the last round
    :param win_score: the score to win
    :param score: present score
    :return: the increment of win
    """
    if chance == 1:
        if score >= win_score:
            return 1
        return 0

    increment = 0
    increment += (chance-1) * take_disc(chance-1, win_score, score)
    increment += take_disc(chance-1, win_score, score+1)
    return increment


def disc_game(n):
    return take_disc(n+1, n//2+1)


def main():
    turns = 15
    win = disc_game(turns)
    total = reduce(mul, range(1, turns + 2))
    print("probability:", win, '/', total)
    print(total // win)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
