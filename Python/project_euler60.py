from mathutil import is_prime, prime_under
import sys


def check_concat_prime(*args):
    for i in range(len(args)):
        for j in range(i+1, len(args)):
            if not is_prime(int(str(args[i])+str(args[j]))) \
               or not is_prime(int(str(args[j])+str(args[i]))):
                return False
    return True


def check_concat_prime_against(old, new):
    for i in old:
        if not is_prime(int(str(i)+str(new))) \
           or not is_prime(int(str(new)+str(i))):
            return False
    return True


def find_solution5(primes):
    max_num_of_prime = len(primes)
    for i in range(1, 10):
        p1 = primes[i]
        for j in range(i+1, max_num_of_prime):
            p2 = primes[j]
            if not check_concat_prime_against([p1], p2):
                continue
            for k in range(j+1, max_num_of_prime):
                p3 = primes[k]
                if not check_concat_prime_against([p1, p2], p3):
                    continue
                for l in range(k+1, max_num_of_prime):
                    p4 = primes[l]
                    if not check_concat_prime_against([p1, p2, p3], p4):
                        continue
                    for m in range(l+1, max_num_of_prime):
                        p5 = primes[m]
                        if check_concat_prime_against([p1, p2, p3, p4], p5):
                            return p1, p2, p3, p4, p5


def main():
    primes = prime_under(10**4)
    primes.remove(5)

    p1, p2, p3, p4, p5 = find_solution5(primes)
    print(p1, p2, p3, p4, p5)
    print(sum([p1, p2, p3, p4, p5]))


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("running time:", time()-starting_time, "seconds")
