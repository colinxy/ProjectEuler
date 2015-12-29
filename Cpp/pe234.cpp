/*
 * poject euler 234: Semidivisible numbers
 *
 */

#include <iostream>
#include <cmath>
#include "mathutil.h"
using namespace std;

using Mathutil::prime_under;
using Mathutil::sum;


const int64_t N = 999966663333L;
const int sqrtN = (int) sqrt(N);


int64_t subsum(int64_t p1, int64_t p2) {
    int64_t p1_sq = p1*p1;
    int64_t p2_sq = p2*p2;

    int64_t p1_sum = sum(p1_sq + p1, p2_sq, p1);
    int64_t p2_sum = sum(p1_sq - p1_sq % p2 + p2, p2_sq - p2, p2);

    return p1_sum + p2_sum - 2 * p1 * p2;
}


int main() {
    vector<int> primes;
    primes.reserve(sqrtN);
    prime_under(primes, sqrtN+1);
    // cout << sqrtN << endl;
    // cout << primes.size() << endl;
    // cout << primes[primes.size()-1] << endl;

    int64_t semidiv_sum(0);

    for (size_t i = 0; i < primes.size()-1; ++i) {
        semidiv_sum += subsum(primes[i], primes[i+1]);
    }

    // last prime 999983
    // first prime after that is 1000003
    // deal with extra
    int64_t p1 = 999983;
    int64_t p2 = 1000003;
    // observation p1 * p2 < N
    semidiv_sum += sum(p1*p1 + p1, N, p1);
    semidiv_sum += sum(p1*p1 - p1*p1%p2 + p2, N, p2);

    cout << semidiv_sum << endl;

    return 0;
}
