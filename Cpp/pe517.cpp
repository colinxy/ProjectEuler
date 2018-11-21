// pe517.cpp


#include <iostream>
#include <vector>
#include <cmath>
#include "mathutil.h"

using namespace std;

using Mathutil::is_prime;
using Mathutil::pow;

const int MOD = 1000000007;
vector<int64_t> mod_inverse_cache(10000);

int64_t g(double x, double a) {
    if (x < a)
        return 1;

    return (g(x-1, a) + g(x-a, a)) % MOD;
}

int64_t G(int x) {
    double a = sqrt(x);

    return g(x, a);
}

int64_t nCr_mod(int n, int k) {
    if (n < k) return 0;

    if (k > n / 2) k = n - k;

    int64_t result = 1;
    for (int i = 1; i <= k; ++i) {
        result = (result * n--) % MOD;
        result = (result * mod_inverse_cache[i]) % MOD;
    }
    return result;
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
// only need to find boundary where g(a,x) = 1,
// then apply binomial coefficient
int64_t G_fast(int x) {
    // ignore cases where x is perfect square
    int64_t result = 0;

    double a = sqrt(x);
    int a_floor = floor(a);

    for (int i = a_floor; i >= 0; i--) {
        // x - (i-1)*a - ones < a
        int ones = floor(x - a * i);

        // sums up  C(n, k) % MOD
        // n = i + ones
        // k = ones
        // cout << i << " " << ones << endl;
        result = (result + nCr_mod(i+ones, ones)) % MOD;
    }

    return result;
}

int main() {
    for (int i = 1; i < (int)mod_inverse_cache.size(); i++) {
        mod_inverse_cache[i] = pow(i, MOD-2, MOD);
    }

    // cout << G(10) << endl;
    // cout << G_fast(10) << endl;
    // cout << G(16) << endl;
    // cout << G_fast(16) << endl;
    // cout << G(90) << endl;
    // cout << G_fast(90) << endl;

    int64_t total = 0;
    for (int i = 10000000; i < 10010000; ++i) {
        if (is_prime(i)) {
            total = (total + G_fast(i)) % MOD;
            cout << i << endl;
        }
    }
    cout << total << endl;

    return 0;
}
