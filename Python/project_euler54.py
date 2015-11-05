from collections import defaultdict


class Poker:
    VAL_dict = {'2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, 
                '8': 8, '9': 9, 'T': 10, 'J': 11, 'Q': 12, 'K': 13, 'A': 14}
    REPR_dict = {j: i for i, j in VAL_dict.items()}

    def __init__(self, card, reverse=False):
        assert len(card) == 2
        self.value, self.suit = tuple(card[i] for i in sorted([0, 1], reverse=reverse))
        self.suit = self.suit.upper()

        if self.suit not in 'SHDCshdc':
            raise NameError('unsupported suit')
        if self.value not in '23456789TJQKA':
            raise NameError('unsupported value')

    def __lt__(self, other):
        return Poker.VAL_dict[self.value] - Poker.VAL_dict[other.value] < 0

    def __gt__(self, other):
        return Poker.VAL_dict[self.value] - Poker.VAL_dict[other.value] > 0

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self):
        return hash((self.value, self.suit))

    def __str__(self):
        return self.value + self.suit

    def __repr__(self):
        return 'Poker(\'' + self.value + self.suit + '\')'


def card_occurrence(iterable):
    value_dict = defaultdict(int)
    for i in iterable:
        value_dict[Poker.VAL_dict[i.value]] += 1
    return value_dict


def royal_flush(hand, val_occurrence):
    if len(set(c.suit for c in hand)) != 1:
        return False
    if [c.value for c in sorted(hand)] != ['T', 'J', 'Q', 'K', 'A']:
        return False
    return [max(val_occurrence)]


def straight_flush(hand, val_occurrence):
    if len(set(c.suit for c in hand)) != 1:
        return False
    min_card = min(val_occurrence.keys())
    for card, seq in zip(sorted(hand), range(min_card, 15)):
        if Poker.VAL_dict[card.value] != seq:
            return False
    return [max(val_occurrence)]


def four_of_a_kind(hand, val_occurrence):
    if len(set(val_occurrence)) != 2:
        return False
    val1, val2 = val_occurrence.keys()
    if not val_occurrence[val1] == 4:
        if not val_occurrence[val2] == 4:
            return False
        else:
            return val2, val1
    return [val1, val2]


def full_house(hand, val_occurrence):
    if len(set(val_occurrence)) != 2:
        return False
    val1, val2 = val_occurrence.keys()
    if not val_occurrence[val1] == 3:
        if not val_occurrence[val2] == 3:
            return False
        else:
            return val2, val1
    return [val1, val2]


def flush(hand, val_occurrence):
    if len(set(c.suit for c in hand)) != 1:
        return False
    return sorted(val_occurrence, reverse=True)


def straight(hand, val_occurrence):
    min_card = min(val_occurrence)
    for card, seq in zip(sorted(hand), range(min_card, 15)):
        if Poker.VAL_dict[card.value] != seq:
            return False
    return [max(val_occurrence)]


def three_of_a_kind(hand, val_occurrence):
    val_occur_rev = {j: i for i, j in val_occurrence.items()}
    if 3 not in val_occur_rev:
        return False
    return [val_occur_rev[3]] + sorted([i for i, j in val_occurrence.items() if j != 3], reverse=True)


def two_pairs(hand, val_occurrence):
    pair = []
    for val, occur in val_occurrence.items():
        if occur == 2:
            pair.append(val)
        else:
            other = val
    if not len(pair) == 2:
        return False
    return sorted(pair, reverse=True) + [other]


def one_pair(hand, val_occurrence):
    other = []
    pair = 0
    for val, occur in val_occurrence.items():
        if occur == 2:
            pair = val
        else:
            other.append(val)
    if pair == 0:
        return False
    return [pair] + sorted(other, reverse=True)


def high_card(hand, val_occurrence):
    return sorted(val_occurrence, reverse=True)


ranking = [royal_flush, straight_flush, four_of_a_kind, full_house, 
           flush, straight, three_of_a_kind, two_pairs, one_pair, high_card]


def win_over(hand1, hand2):
    val_occur1 = card_occurrence(hand1)
    val_occur2 = card_occurrence(hand2)

    for rank in ranking:
        result1 = rank(hand1, val_occur1)
        result2 = rank(hand2, val_occur2)
        if result1 and result2:
            for r1, r2 in zip(result1, result2):
                if r1 > r2:
                    # print(rank.__name__, r1)
                    return True
                if r2 > r1:
                    # print(rank.__name__, r2)
                    return False
        if result1 and not result2:
            # print(rank.__name__)
            return True
        if not result1 and result2:
            # print(rank.__name__)
            return False


def main():
    # test = "5H 5C 6S 7S KD 2C 3S 8S 8D TD\n" \
    #        "5D 8C 9S JS AC 2C 5C 7D 8S QH\n" \
    #        "2D 9C AS AH AC 3D 6D 7D TD QD\n" \
    #        "4D 6S 9H QH QC 3D 6D 7H QD QS\n" \
    #        "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
    # for line in test.splitlines():
    #     two_hands = [Poker(c) for c in line.split()]
    #     print(win_over(two_hands[:5], two_hands[5:]))

    count = 0
    with open('p054_poker.txt') as f:
        for line in f:
            two_hands = [Poker(c) for c in line.split()]
            hand1, hand2 = two_hands[:5], two_hands[5:]
            # print([str(c) for c in hand1], [str(c) for c in hand2])
            # print(list(map(str, sorted(hand1))), list(map(str, sorted(hand2))))
            if win_over(hand1, hand2):
                # print(1)
                count += 1
            # print()
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
