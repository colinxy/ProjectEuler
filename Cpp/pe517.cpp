// pe517.cpp


#include <iostream>
#include <vector>
#include <cmath>
#include "mathutil.h"

using namespace std;

using Mathutil::is_prime;

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

int64_t G_fast(int x) {
    // ignore cases where x is perfect square

    // pascal's triangle
    // one side -1, the other side -a
    double a = sqrt(x);
    int a_floor = floor(a);

    return 0;
}

int main() {
    cout << G(90) << endl;

    vector<int> primes;
    for (int i = 10000000; i < 10010000; ++i) {
        if (is_prime(i)) {
            primes.push_back(i);
        }
    }
    cout << primes.size() << endl;
    // cout << G(primes[0]) << endl;

    return 0;
}
