# Roman numerals
"""
For a number written in Roman numerals to be considered valid 
there are basic rules which must be followed. Even though
the rules allow some numbers to be expressed in more than one way 
there is always a "best" way of writing a particular
number.

For example, it would appear that there are at least six ways of 
writing the number sixteen:

IIIIIIIIIIIIIIII
VIIIIIIIIIII
VVIIIIII
XIIIIII
VVVI
XVI

However, according to the rules only XIIIIII and XVI are valid, 
and the last example is considered to be the most
efficient, as it uses the least number of numerals.

The 11K text file, roman.txt, contains one thousand numbers written in valid,
but not necessarily minimal, Roman numerals;

Find the number of characters saved by writing each of these 
in their minimal form.

Note: You can assume that all the Roman numerals in the file contain 
no more than four consecutive identical units.
"""

import re

roman = ['I', 'V', 'X', 'L', 'C', 'D', 'M']
roman_num = {'I': 1, 'V': 5, 'X': 10, 'L': 50, 'C': 100, 'D': 500, 'M': 1000}
PATTERN = re.compile(r"^(M*)((?:CM)|(?:CD)|(?:DC*)|C*)((?:XC)|(?:XL)|(?:LX*)|X*)((?:IX)|(?:IV)|(?:VI*)|I*)$")


def parse_roman(s):
    result = PATTERN.match(s)
    return [result.group(i) for i in range(1, 5)]


def roman_minimize(s_list):
    # convert_dict = {100: 'C', 200: 'CC', 300: 'CCC', 400: 'CD', 500:'D', 600: 'DC', 700: 'DCC', 800: 'DCCC', 900: 'CM'}
    if s_list[1] == "CCCC":
        s_list[1] = "CD"
    elif s_list[1] == "DCCCC":
        s_list[1] = "CM"

    if s_list[2] == "XXXX":
        s_list[2] = "XL"
    elif s_list[2] == "LXXXX":
        s_list[2] = "XC"

    if s_list[3] == "IIII":
        s_list[3] = "IV"
    elif s_list[3] == "VIIII":
        s_list[3] = "IX"

    return s_list


def main():
    saved_total = 0

    with open("p089_roman.txt") as f:
        for line in f:
            line = line.rstrip('\n')

            s_list = parse_roman(line)
            s_list = roman_minimize(s_list)
            line_minimized = ''.join(s_list)
            # print(line, line_minimized)
            saved = len(line) - len(line_minimized)
            saved_total += saved

    print(saved_total)


if __name__ == "__main__":
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
