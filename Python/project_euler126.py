
def cover_cuboid_top(cuboid, top):
    surface = 2 * (cuboid[0]*cuboid[1] + cuboid[0]*cuboid[2] + cuboid[1]*cuboid[2])
    edge = 4 * sum(cuboid)
    layers = []

    curr = surface
    diff = edge
    while curr <= top:
        layers.append(curr)
        curr += diff
        diff += 8

    return layers


def main():
    # print(cover_cuboid_top((1, 2, 3), 200))

    NUM_SOLUTION = 1000
    N = 20000

    num_solutions = [0] * (N+1)
    for i in range(1, int((N/3) ** 0.5)):
        for j in range(i, int(N ** 0.5)):
            for k in range(j, N // 2):
                layers = cover_cuboid_top((i, j, k), N)
                # print(layers)
                if not layers:
                    break
                for layer in layers:
                    num_solutions[layer] += 1

    # for i, j in enumerate(num_solutions): print(i, j)
    # print(max((j, i) for i, j in enumerate(num_solutions)))

    for index, num_solution in enumerate(num_solutions):
        if num_solution == NUM_SOLUTION:
            print(index)
            break


if __name__ == '__main__':
    from time import time
    starting_time = time()
    main()
    print("Time elapsed:", time() - starting_time, "seconds")
