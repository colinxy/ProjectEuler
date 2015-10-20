
from project_euler105 import is_special


def main():
    optimum = sum([20, 31, 38, 39, 40, 42, 45])
    set_str = [20, 31, 38, 39, 40, 42, 45]

    for i in range(19, 23):
        for j in range(i+1, 50):
            for k in range(j+1, 50):
                for l in range(k+1, 50):
                    for m in range(l+1, 50):
                        for n in range(m+1, 50):
                            for o in range(n+1, 51):
                                the_set = [i, j, k, l, m, n, o]
                                if is_special(the_set):
                                    if sum(the_set) < optimum:
                                        optimum = sum(the_set)
                                        set_str = the_set
                                        print(optimum, set_str)
    print(optimum, set_str)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
