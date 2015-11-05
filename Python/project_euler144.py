
from math import sqrt


# todo: improve class Ellipse and Linear and put them in CAS
class Ellipse(object):
    # todo: change implementation to second form
    def __init__(self, a, b):
        """
        eclipse of the form ax^2 + y^2 = b
        eclipse of the form x^2 / a^2 + y^2 / b^2 = 1
        """
        self.a = a
        self.semi_major_axis = sqrt(b)

    def __str__(self):
        return "{0:.3f}x^2 + y^2 = {1:.3f}".format(self.a, self.semi_major_axis**2)

    def semi_major_axis(self):
        return self.semi_major_axis

    def semi_minor_axis(self):
        return sqrt(self.semi_major_axis**2 / self.a)

    def linear_eccentricity(self):
        return sqrt(self.semi_major_axis**2 - self.semi_minor_axis()**2)

    def eccentricity(self):
        return self.linear_eccentricity() / self.semi_major_axis

    def on_the_ellipse(self, x, y):
        v = (x / self.semi_minor_axis())**2 + (y / self.semi_major_axis)**2
        # print(x, y, v, sep='\n')
        return 0.999999 < v < 1.000001

    def solve_for_y(self, x):
        if abs(x) > self.semi_minor_axis():
            raise ValueError("not within domain")
        y = sqrt(self.semi_major_axis**2 - self.a*x**2)
        return y, -y

    def solve_for_x(self, y):
        if abs(y) > self.semi_major_axis:
            raise ValueError("not within domain")
        x = sqrt((self.semi_major_axis**2 - y**2) / self.a)
        return x, -x

    def tangent(self, x0, y0):
        """
        tangent is of the form: x*x0/a^2 + y*y0/b^2 = 1
        """
        a_squared = self.semi_minor_axis() ** 2
        b_squared = self.semi_major_axis ** 2
        linear_a = - x0*b_squared / (y0*a_squared)
        linear_b = b_squared / y0
        return Linear(linear_a, linear_b)

    def intersect_with_linear(self, linear, x=None, adjust=False):
        """
        solve the x value of the intersection
        """
        a, b, c = (self.a + linear.a**2), (2 * linear.a * linear.b), (linear.b**2 - self.semi_major_axis**2)
        if x is not None and not adjust:
            x2_1 = Ellipse.vieta1(a, b, x)
            # x2_2 = Ellipse.vieta2(a, c, x)
            return x2_1, linear.solve_for_y(x2_1)

        x1, x2 = Ellipse.solve_quad(a, b, c)
        if x is not None:
            epsilon = 1e-6
            if abs(x - x1) < epsilon:
                return x2, linear.solve_for_y(x2)
            elif abs(x - x2) < epsilon:
                return x1, linear.solve_for_y(x1)
            else:
                raise ValueError("provided x value is not the intersection point")
        return x1, linear.solve_for_y(x1), x2, linear.solve_for_y(x2)

    @staticmethod
    def solve_quad(a, b, c):
        """
        solve quadratic function of the form ax^2 + bx + c = 0
        """
        delta = b**2 - 4*a*c
        if delta < 0:
            raise ValueError("delta < 0")
        return (-b + sqrt(delta)) / (2*a), (-b - sqrt(delta)) / (2*a)

    @staticmethod
    def vieta1(a, b, x1):
        return -b / a - x1

    @staticmethod
    def vieta2(a, c, x1):
        return c / a / x1


class Linear(object):
    def __init__(self, a, b):
        """
        linear function of the form y = ax + b
        """
        self.a = a
        self.b = b

    @classmethod
    def from_points(cls, x1, y1, x2, y2):
        a = (y2 - y1) / (x2 - x1)
        b = (y1*x2 - y2*x1) / (x2 - x1)
        return cls(a, b)

    @classmethod
    def from_slop(cls, a, x1, y1):
        b = y1 - a*x1
        return cls(a, b)

    @staticmethod
    def point_symmetry(sym_x, sym_y, x, y):
        return 2*sym_x - x, 2*sym_y - y

    def __str__(self):
        return "y = {a:.3f}x + {b:.3f}".format(a=self.a, b=self.b)

    def solve_for_y(self, x):
        return self.a * x + self.b

    def solve_for_x(self, y):
        return (y - self.b) / self.a

    def intersect(self, other):
        x = (other.b - self.b) / (self.a - other.a)
        y = (self.a*other.b - other.a*self.b) / (self.a - other.a)
        return x, y

    def vertical_at(self, x):
        a = -1 / self.a
        y = self.solve_for_y(x)
        return Linear.from_slop(a, x, y)

    def reflect(self, reflection_line):
        intersect_x, intersect_y = self.intersect(reflection_line)
        sym_x, sym_y = intersect_x-1, Linear.solve_for_y(reflection_line, intersect_x-1)
        vertical_line = Linear.vertical_at(reflection_line, intersect_x-1)
        vertical_x, vertical_y = self.intersect(vertical_line)
        reflect_x, reflect_y = Linear.point_symmetry(sym_x, sym_y, vertical_x, vertical_y)

        return Linear.from_points(intersect_x, intersect_y, reflect_x, reflect_y)


def main():
    beam = Linear.from_points(0, 10.1, 1.4, -9.6)
    white_cell = Ellipse(4, 100)

    current = beam
    intersect_x, intersect_y = 1.4, -9.6
    count = 1
    while True:
        tangent = white_cell.tangent(intersect_x, intersect_y)
        # print("current: {}".format(current), "No.{0} hit: {1:.3f}, {2:.3f}".format(count, intersect_x, intersect_y), "tangent: {}".format(tangent), sep='\n', end='\n\n')
        current = current.reflect(tangent)
        intersect_x, intersect_y = white_cell.intersect_with_linear(current, intersect_x, True)
        if not white_cell.on_the_ellipse(intersect_x, intersect_y):
            raise ArithmeticError

        if -0.01 <= intersect_x <= 0.01 and intersect_y > 0:
            # print(intersect_x, intersect_y)
            break

        count += 1

    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
