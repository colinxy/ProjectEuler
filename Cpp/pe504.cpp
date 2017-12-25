// pe504


#include <iostream>
#include <cmath>

using namespace std;

const int M = 100;

int lattice_cache[M+1][M+1];

// how many lattice points strictly in triangle
// (0,0), (0,a), (b,0)
int lattice_points(int a, int b) {
    int count = 0;
    for (int i = 1; i < a; i++) {
        if (b * i % a == 0) {
            count += b - b * i / a - 1;
        } else {
            count += (int)(b - b * i / (double)a);
        }
    }
    return count;
}

int lattice_points(int a, int b, int c, int d) {
    return (a+c-1) + (b+d-1) - 1 +
        lattice_cache[a][b] +
        lattice_cache[b][c] +
        lattice_cache[c][d] +
        lattice_cache[d][a];
        // lattice_points(a, b) +
        // lattice_points(b, c) +
        // lattice_points(c, d) +
        // lattice_points(d, a);
}

bool is_square(int n) {
    int rt = (int)sqrt(n);
    return rt*rt == n;
}

int brute_force(int M) {
    int count = 0;
    for (int a = 1; a <= M; a++) {
        for (int b = 1; b <= M; b++) {
            for (int c = 1; c <= M; c++) {
                for (int d = 1; d <= M; d++) {
                    int lp = lattice_points(a, b, c, d);
                    if (is_square(lp)) {
                        // cout << a << ' ' << b << ' ' << c << ' ' << d << endl;
                        // cout << lp << endl;
                        count++;
                    }
                }
            }
        }
    }
    return count;
}

int main() {
    memset(lattice_cache, 0, (M+1)*(M+1));
    for (int i = 1; i <= M; i++) {
        for (int j = i; j <= M; j++) {
            int lp = lattice_points(i, j);
            lattice_cache[i][j] = lp;
            lattice_cache[j][i] = lp;
        }
    }
    cout << brute_force(M) << endl;

    return 0;
}
