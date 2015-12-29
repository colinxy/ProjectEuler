"""
Sum: (ak+b) * r**(k-1), from 1 to n

(an+b) * r**n - (a+b) - a * ((1 - r**n) / (1 - r) - 1)
"""


def s5000(r):
    return (-14100*r**5000 - 897 + 3 * ((1-r**5000) / (1-r) - 1)) / (r-1)


def main():
    target = -6e11
    epsilon = 1e0
    digits = 12
    
    start = 1.0001 # > target
    end   = 1.1    # < target

    for _ in range(10000):
        mid = (start + end) / 2
        s_mid =  s5000(mid)

        # print(s_mid, mid)

        if abs(s_mid - target) < epsilon:
            print("{:.{d}f}".format(mid, d=digits))
            # print(s_mid, start, end, sep='\n')
            break
        if s_mid > target:
            start = mid
        else:
            end = mid


if __name__ == '__main__':
    main()

