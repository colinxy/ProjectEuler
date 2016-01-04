// pe517.cpp


#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include "mathutil.h"
#include "double.h"

using namespace std;

using Mathutil::is_prime;

const int MOD = 1000000007;

double a;
// int cache[11000000];
unordered_map<Double, int> cache_map;


int64_t g(double x) {
    if (cache_map[x])
        return cache_map[x];

    if (x < a)
        cache_map[x] = 1;
    else
        cache_map[x] = (g(x-1) + g(x-a)) % MOD;

    return cache_map[x];
}

/*
int64_t g(int x) {
    if (cache[x] > 0) return cache[x];

    if (x < a) {
        cache[x] = 1;
        return 1;
    }

    cache[x] = ((g(x-1) + g(x - a)) % MOD);

    return cache[x];
}*/

int64_t G(int x) {
    a = sqrt(x);

    // memset(cache, 0, x * sizeof(int));

    return g(x);
}

int main() {
    // cout << G(10000) << endl;

    vector<int> primes;
    for (int i = 10000000; i < 10010000; ++i) {
        if (is_prime(i)) {
            primes.push_back(i);
        }
    }
    // cout << primes.size() << endl;

    return 0;
}
