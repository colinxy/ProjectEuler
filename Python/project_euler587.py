# Problem 587: Concave triangle

# (1-sin(x)) / (1-cos(x)) = n
# area = (1 - cos(x)) (1 - sin(x)) / 2 + (2 - cos(x)) sin(x) / 2 - x / 2

from sympy import symbols, nsolve, sin, cos, pi, ceiling


def concave_triangle(area):
    theta, n = symbols('theta n')
    A = (1-cos(theta)) * (1-sin(theta)) / 2 + \
        (2-cos(theta)) * sin(theta) / 2 - \
        theta / 2
    theta_ = nsolve(A-area, theta, 0.1)
    # print(theta_)
    n = nsolve((1-sin(theta_)) - (1-cos(theta_))*n, n, 15)
    return n


def main():
    L = 1 - pi / 4

    print(ceiling(concave_triangle(0.001 * L)))


if __name__ == '__main__':
    main()
