// pe225.cpp

/*
 * project euler 225: Tribonacci non-divisors
 *
 * | t[n+3] |     | 1 1 1 |   | t[n+2] |
 * | t[n+2] |  =  | 1 0 0 | * | t[n+1] |
 * | t[n+1] |     | 0 1 0 |   | t[n]   |
 *
 * T[k+1] = A * T[k]
 * T[n] = A^n * T[0]
 *
 * compute A eigenvalue:
 * | A - lI | = 0
 * p(l) = -l^3 + l^2 + l + 1 = 0
 *
 */


#include <iostream>
using namespace std;


const int N = 124;

int main() {
    int count = 0;
    int64_t div = 1;

    while (count < N) {
        div += 2;

        int a = 1, b = 1, c = 1;
        while (true) {
            int temp_a = a, temp_b = b;
            a = (a+b+c) % div;
            b = temp_a % div;
            c = temp_b % div;

            if (a == 0) break;
            if (a == 1 && b == 1 && c == 1) {
                ++count;
                // cout << count << ' ' << div << endl;
                break;
            }
        }
    }

    cout << div << endl;

    return 0;
}
