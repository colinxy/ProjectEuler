# refer to https://en.wikipedia.org/wiki/Nim
#
# X(n1, n2, n3) == 0
# iff
# n1 xor n2 xor n3 == 0

# n xor 2n xor 3n == 0
# ==>
# binary representation of n does not have consecutive 1s


def binrep(k):
    """
    k binary digits
    """
    if k == 0:
        return 1
    if k == 1:
        # 1, 0
        return 2
    if k == 2:
        # 10, 01, 00
        return 3

    # 10___
    # 010__
    # 00___
    return binrep(k-2) * 2 + binrep(k-3)


# including 2**30, excluding 0
print(binrep(30))
