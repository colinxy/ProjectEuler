# project euler 84: Monopoly odds
# use simulation

from __future__ import division
from random import randint

DICE = 4
TRIAL = 10 ** 6
grids = [0] * 40

cc_deck = [0, 10] + [-1] * 14
cc_top = 0

ch_deck = [0, 10, 11, 24, 39, 5,
           lambda x: next_rail(x),
           lambda x: next_rail(x),
           lambda x: next_utility(x),
           lambda x: (x-3) % 40] + \
           [-1]*6
ch_top = 0


def next_rail(pos):
    rails = [5, 15, 25, 35]
    for i in range(len(rails)):
        if pos < rails[i]:
            return rails[i]
    return rails[0]


def next_utility(pos):
    utils = [12, 28, 12]
    for i in range(len(utils)):
        if pos < utils[i]:
            return utils[i+1]
    return utils[0]


def comm_chest(pos):
    global cc_top

    if cc_deck[cc_top] == -1:
        cc_top = (cc_top + 1) % 16
        return pos

    new_pos = cc_deck[cc_top]
    cc_top = (cc_top + 1) % 16
    return new_pos


def chance(pos):
    global ch_top

    if ch_deck[ch_top] == -1:
        ch_top = (ch_top + 1) % 16
        return pos

    new_pos = ch_deck[ch_top](pos) if callable(ch_deck[ch_top]) \
        else ch_deck[ch_top]
    ch_top = (ch_top + 1) % 16
    return new_pos


def check_trick(pos):
    # go to jail
    if pos == 30:
        return 10
    # community chest
    if pos in [2, 17, 33]:
        return comm_chest(pos)
    # chance
    if pos in [7, 22, 36]:
        return chance(pos)

    return pos


def advance(curr_pos):
    first = randint(1, DICE)
    second = randint(1, DICE)
    curr_pos = (curr_pos + first + second) % 40

    new_pos = check_trick(curr_pos)
    if new_pos != curr_pos:
        grids[new_pos] += 1
        return new_pos
    if first != second:
        grids[curr_pos] += 1
        return curr_pos

    first = randint(1, DICE)
    second = randint(1, DICE)
    curr_pos = (curr_pos + first + second) % 40

    new_pos = check_trick(curr_pos)
    if new_pos != curr_pos:
        grids[new_pos] += 1
        return new_pos
    if first != second:
        grids[curr_pos] += 1
        return curr_pos

    first = randint(1, DICE)
    second = randint(1, DICE)
    if first == second:
        grids[10] += 1          # jail
        return 10

    curr_pos = (curr_pos + first + second) % 40
    grids[curr_pos] += 1
    return curr_pos


def main():
    curr_pos = 0
    for i in range(TRIAL):
        curr_pos = advance(curr_pos)

    # print(grids)
    print(sorted(enumerate(i/TRIAL for i in grids), key=lambda x: x[1])[-3:])


if __name__ == '__main__':
    main()
