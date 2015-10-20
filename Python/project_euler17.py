# Number letter counts
"""
If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were 
written out in words, how many letters would be used?
"""

DIGITS = ["zero", "one", "two", "three", "four", 
          "five", "six", "seven", "eight", "nine"]
TENS = ["twenty", "thirty", "forty", "fifty", 
        "sixty", "seventy", "eighty", "ninety"]
BWN10_20 = ["ten", "eleven", "twelve", "thirteen", "fourteen", 
            "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]


def in_word_under1000(x):
    in_word = []

    if x < 10:
        in_word.append(DIGITS[x])
    elif 10 <= x < 20:
        in_word.append(BWN10_20[x-10])
    elif 20 <= x < 100:
        word = TENS[x//10-2]
        if x % 10 != 0:
            word += DIGITS[x%10]
        in_word.append(word)
    else:
        in_word.append(DIGITS[x//100])
        in_word.append("hundred")
        under100 = x % 100
        if under100 != 0:
            in_word.append("and")
            in_word.extend(in_word_under1000(under100))

    return in_word


def main():
    total = 0
    for i in range(1, 1000):
        words = in_word_under1000(i)
        # print(words)
        total += len(''.join(words))
    total += len("onethousand")

    print(total)


if __name__ == '__main__':
    main()
