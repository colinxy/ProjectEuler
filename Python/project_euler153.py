
import numpy as np
from mathutil import gcd
from math import sqrt


def main():
    N = 10 ** 8
    sq_coprime_sum = np.zeros((N + 1,), dtype=np.uint32)
    result_real = np.zeros((N + 1,), dtype=np.uint32)
    result_imag = np.zeros((N + 1,), dtype=np.uint32)
    print("successfully allocated")

    sq_coprime_sum[2] = 2  # 1*1 + 1*1 = 2
    for i in range(1, int(sqrt(N)) + 1):
        for j in range(i + 1, int(sqrt(N)) + 1):
            squared_sum = i * i + j * j
            if squared_sum > N:
                break
            if gcd(i, j) == 1:
                sq_coprime_sum[squared_sum] += 2 * (i + j)
                # print(i, j, squared_sum)
    # print(sq_coprime_sum)
    print("sq_coprime_sum computed by", time() - starting_time)

    for i in range(1, N + 1):
        for j in range(i, N + 1, i):
            result_real[j] += i
    print("real part computed by", time() - starting_time)

    for i in range(1, N + 1):
        if sq_coprime_sum[i] == 0:
            continue
        for j in range(i, N + 1, i):
            result_imag[j] += sq_coprime_sum[i] * result_real[j // i]
    print("complex part computed by", time() - starting_time)

    # print(list(zip(result_real, result_imag)))
    print(sum(map(int, result_real)) + sum(map(int, result_imag)))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()  # slow, 25 mins
    print("Time elapsed:", time() - starting_time, "seconds")
