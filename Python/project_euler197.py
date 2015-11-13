def f(x):
    return int(2 ** (30.403243784-x*x)) / 1000000000


def main():
    trial = 10**8
    digits = 9
    
    u_even = -1
    prev_even_str = "{0:.{d}f}".format(u_even, d=digits)
    
    for i in range(trial):
        u_odd = f(u_even)
        u_even = f(u_odd)
        # print(u)
        if prev_even_str == "{0:.{d}f}".format(u_even, d=digits):
            # print(i)
            print("{0:.{d}f}".format(u_even + f(u_even), d=digits))
            break
        else:
            prev_even_str = "{0:.{d}f}".format(u_even, d=digits)


if __name__ == '__main__':
    main()
