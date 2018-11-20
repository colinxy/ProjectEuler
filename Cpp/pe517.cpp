// pe517.cpp


#include <iostream>
#include <vector>
#include <cmath>
#include "mathutil.h"

using namespace std;

using Mathutil::is_prime;
using Mathutil::pow;

const int MOD = 1000000007;

int64_t g(double x, double a) {
    if (x < a)
        return 1;

    return (g(x-1, a) + g(x-a, a)) % MOD;
}

int64_t G(int x) {
    double a = sqrt(x);

    return g(x, a);
}

// think recursion as following pascal's triangle
// as you goes down the triangle,
// going down left ridge -1, going down right ridge -a
/*
      X
  -1     -a
    X   X
-2   -1-a  -2a
  X   X   X
      -2-a   -3a
X   X   X   X
 */
// only need to compute one row of pascal triangle,
// then apply binomial coefficient
int64_t G_fast(int x) {
    // ignore cases where x is perfect square

    int64_t result = 0;

    double a = sqrt(x);
    int a_floor = floor(a);

    vector<int64_t> row(a_floor+1);
    int ri = a_floor;

    // x - a*a_floor
    row[ri--] = 1;
    vector<int64_t> col;
    vector<int64_t> col_next;
    col.reserve(x);
    col_next.reserve(x);
    col.push_back(1);

    for (int i = a_floor - 1; i >= 0; i--) {
        // x - 1*(a_floor-i) - a*i
        // ends at: x - 1*end_ones - a*i
        int start_ones = a_floor - i;
        int end_ones = ceil(x - a * (i+1));
        int length = end_ones - start_ones + 1;

        col_next.resize(length);
        for (int j = length-1; j >= (int)col.size()-1; j--) {
            col_next[j] = length - j;
        }
        for (int j = col.size()-1; j >= 1; j--) {
            col_next[j-1] = (col_next[j] + col[j]) % MOD;
        }

        // cout << i << " " << col_next[0] << " " << col_next.size() << endl;
        row[ri--] = col_next[0];

        swap(col, col_next);
    }

    result = row[0];

    int64_t combin = 1;
    int n = a_floor;
    for (int i = 1; i <= a_floor; i++) {
        // find modular inverse of i
        // method 1: fermat's little theorem
        // a^(p-1) = 1 (mod p)
        // a^(-1) = a^(p-2) (mod p)
        combin = (combin * n * pow(i, MOD-2, MOD)) % MOD;
        n--;
        // TODO: implement extended Euclidean algorithm to find
        // modular inverse

        result = (result + row[i] * combin % MOD) % MOD;
    }

    return result;
}

int main() {
    cout << G(90) << endl;
    cout << G_fast(90) << endl;

    vector<int> primes;
    for (int i = 10000000; i < 10010000; ++i) {
        if (is_prime(i)) {
            primes.push_back(i);
        }
    }
    cout << primes.size() << endl;
    cout << primes[primes.size()-1] << endl;
    cout << G_fast(primes[primes.size()-1]) << endl;

    return 0;
}
