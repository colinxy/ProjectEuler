# project euler 109: Darts

from __future__ import division

singles = set(range(1, 21)) | {25}
encoding = {1: "S", 2: "D", 3: "T"}
TOP = 100


def checkout(n):
    count = 0
    for last in sorted(singles):
        left2 = n - 2*last
        if left2 == 0:
            count += 1
            break
        if left2 < 0:
            break

        checkout_seq = set()
        for c in [1, 2, 3]:
            for i in sorted(singles):
                # 25 has no treble
                if i == 25 and c == 3:
                    continue

                left1 = left2 - i * c
                if left1 == 0:
                    count += 1
                    break
                if left1 < 0:
                    break

                # duplicate in frozenset, but does not affect answer
                if left1 in singles:
                    checkout_seq.add(frozenset(("S"+str(left1),
                                                encoding[c]+str(i))))
                if left1 % 2 == 0 and left1 // 2 in singles:
                    checkout_seq.add(frozenset(("D"+str(left1//2),
                                                encoding[c]+str(i))))
                # 25 has no treble
                if left1 % 3 == 0 and 1 <= left1 // 3 <= 20:
                    checkout_seq.add(frozenset(("T"+str(left1//3),
                                                encoding[c]+str(i))))
        count += len(checkout_seq)
        # print(last, checkout_seq)

    return count


def main():
    # print(checkout(6))
    # print(checkout(170))
    # print(sum(checkout(i) for i in range(2, 171)))
    print(sum(checkout(i) for i in range(2, TOP)))


if __name__ == '__main__':
    main()
