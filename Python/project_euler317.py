# Firecracker

# y = h + v sin(a) t - g/2 t^2
# t = x/(vcos(a))
# y(t) = h + v sin(a) x/cos(a) - g/2 x^2/(v^2 cos(a)^2)
# GOAL: max{y(t) : y(t) > 0}

# let z = tan(a)
# y = h + vx z + g/2 x^2/v^2 (z^2+1)
#   = (h-g/2 x^2/v^2) + x z - g/2 x^2/v^2 z^2

# largest possible y achieved when
#     z0 = v^2/(gx)
# largest possible x achieved when delta = 0
#     x0 = sqrt((1 + 2gh/v^2) v^4/g^2)

# careful with cylindrical volume integration
# integrate(y(z) (2pi x) dx, 0, x0)

from sympy import symbols, sqrt, pi, integrate

x, z = symbols('x z')


def solve(h, v, g):
    y = (h - g/2 * x**2/v**2) + x*z - g/2 * x**2/v**2 * z**2
    z0 = v**2 / (g*x)
    x0 = sqrt((1 + 2*g*h/v**2) * v**4/g**2)

    y_z = y.subs(z, z0)         # y(z) is a function of x
    print(y_z)

    return integrate(y_z * (2*pi * x), (x, 0, x0)).evalf()


def main():
    h, v, g = symbols('h v g')
    solve(h, v, g)
    print(solve(h=100, v=20, g=9.81))


if __name__ == '__main__':
    main()
