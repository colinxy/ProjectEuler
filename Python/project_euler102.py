# Triangle containment
"""
Three distinct points are plotted at random on a Cartesian plane, 
for which -1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:

A(-340,495), B(-153,-910), C(835,-947)

X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, 
whereas triangle XYZ does not.

Using triangles.txt (right click and 'Save Link/Target As...'), 
a 27K text file containing the co-ordinates of one thousand 
"random" triangles, find the number of triangles for which 
the interior contains the origin.

NOTE: The first two examples in the file represent 
the triangles in the example given above.
"""


def area_with_origin(x1, y1, x2, y2):
    return abs(x1*y2 - x2*y1) / 2


def area(x1, y1, x2, y2, x3, y3):
    return area_with_origin(x2-x1, y2-y1, x3-x1, y3-y1)


def contains_origin(x1, y1, x2, y2, x3, y3):
    a = area(x1, y1, x2, y2, x3, y3)
    b = area_with_origin(x1, y1, x2, y2) + \
        area_with_origin(x1, y1, x3, y3) + \
        area_with_origin(x2, y2, x3, y3)
    return a == b


def main():
    count = 0
    with open("p102_triangles.txt") as f:
        for line in f:
            x1, y1, x2, y2, x3, y3 = [int(s) for s in line.split(',')]
            if contains_origin(x1, y1, x2, y2, x3, y3):
                count += 1
    print(count)


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
