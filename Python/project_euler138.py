# half_b = 2mn 
# h = (m^2-n^2)
# L = (m^2+n^2)
# 
# 4mn +- 1 = m^2-n^2
# (m-2n)^2 - 5n^2 = +-1
# 
# x = m-2n
# y = n
# x^2 - 5y^2 = +-1

import pell

N = 12


def get_area_l(x, y):
    n = y
    m = x + n * 2
    
    half_b = 2 * m * n
    h = m * m - n * n
    l = m * m + n * n
    return h * half_b, l


def main():
    areas_l = set()

    # fundamental solution to x^2 - 5y^2 = 1
    x, y = pell.pell(5)
    first_x, first_y = x, y
    areas_l.add(get_area_l(x, y))

    for _ in range(N):
        x, y = pell.next_solution(5, first_x, first_y, x, y)
        areas_l.add(get_area_l(x, y))
        
    # fundamental solution to x^2 - 5y^2 = -1
    x, y = pell.pell(5, True)
    first_x, first_y = x, y
    areas_l.add(get_area_l(x, y))

    for _ in range(N):
        x, y = pell.next_solution(5, first_x, first_y, x, y)
        x, y = pell.next_solution(5, first_x, first_y, x, y)
        areas_l.add(get_area_l(x, y))

    areas_l = sorted(areas_l)
    print(sum(areas_l[i][1] for i in range(N)))


if __name__ == '__main__':
    main()
