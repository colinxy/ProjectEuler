from math import sqrt
from itertools import permutations


def is_square(x):
    return x == int(sqrt(x)) ** 2


def form_num(word, char_map):
    return int(''.join(char_map[char] for char in word))


def main():
    with open("p098_words.txt") as f:
        words = eval('[' + f.read() + ']')

    sorted_dict = {}
    for w in words:
        w_sorted = ''.join(sorted(w))
        sorted_dict.setdefault(w_sorted, []).append(w)

    anagram = {}
    for w_sorted, words in sorted_dict.items():
        if len(words) > 1:  # more than 1 permutation
            anagram.setdefault(len(w_sorted), []).append(words)
    # print(anagram)
    max_len = max(anagram.keys())

    found = False
    solution = []
    for word_len in range(max_len, 0, -1):
        if word_len not in anagram:
            continue
        for group in anagram[word_len]:
            # print(group)
            char_set = sorted(set(group[0]))
            combination = permutations('0123456789', len(char_set))

            for com in combination:
                char_map = dict(zip(char_set, com))
                if all(char_map[word[0]] != '0' and   # number does not start with 0
                       is_square(form_num(word, char_map))
                       for word in group):

                    numbers = [form_num(word, char_map) for word in group]
                    print(numbers, group)
                    solution.extend(numbers)
                    found = True
        if found:
            break

    print(max(solution))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # slow, 30s
    print("Time elapsed:", time() - starting_time, "seconds")
