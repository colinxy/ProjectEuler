from math import sqrt
from decimal import Decimal, getcontext


def fib(mod=10**9):
    a, b = 1, 1
    while True:
        yield a
        a, b = b%mod, (a + b)%mod


def fib_approx(k, phi=Decimal(str(sqrt(5)/2+1/2)), 
                  psi=Decimal(str(1/2-sqrt(5)/2)), 
                  diff=Decimal(str(sqrt(5)))):
    return (phi ** k - psi ** k) / diff


def is_pan(x_str, pan="123456789"):  # x guaranteed to have at most number of digits as pan
    return set(x_str) == set(pan)


def main():
    getcontext().prec = 15

    k = 0
    fibs = iter(fib())
    while True:
        f_k = next(fibs)
        k += 1
        f_k_str = str(f_k)
        if is_pan(f_k_str[-9:]):  # and is_pan(f_k_str[:9]):
            # print(k, f_k, end=' ')
            actual_f_k = str(fib_approx(k))
            first9 = actual_f_k[0] + actual_f_k[2:10]  # omit the decimal point
            # print(first9)
            if is_pan(first9):
                print(k, f_k, actual_f_k)
                print(k)
                break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
