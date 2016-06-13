from matplotlib import pylab
import numpy as np
from sympy import S
from sympy.stats import Binomial, density

"""
f
x: number of times that lose
idea:
    capital = (1+2*f)**x * (1-f)**(1000-x)
    capital >= 10**9
    minimize x
"""


@np.vectorize
def func(f, x):
    return (1 + 2 * f) ** x * (1 - f) ** (1000 - x)


def plot(x):
    f = np.linspace(0, 1, 10000)
    pylab.plot(f, func(f, x))
    pylab.title(str(x))
    pylab.show()


def main():
    # research
    # for x in range(430, 433):
    #     plot(x)
    # research conclude that when x >= 432, capital > 10**9

    x = 432
    bin_density = density(Binomial('X', 1000, S.Half)).dict
    prob = sum(bin_density[i] for i in range(x, 1001))
    print(prob.evalf(12))


if __name__ == '__main__':
    main()
