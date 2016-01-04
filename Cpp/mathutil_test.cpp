// mathutil_test.cpp


#include <iostream>
#include <algorithm>
#include <cassert>
#include "mathutil.h"

using namespace std;
using namespace Mathutil;

int main() {
    // sum

    // pow(, )

    // pow(, , )

    // gcd

    // nCr

    // nCr_s

    // is_prime
    size_t primes[] = {37, 101, 947, 7873, 2147483647};
    for (size_t i = 0; i < sizeof(primes)/sizeof(size_t); ++i) {
        if (!is_prime(primes[i]))
            cerr << "fail to detect " << primes[i] << endl;
    }

    // miller_rabin
    for (size_t i = 0; i < sizeof(primes)/sizeof(size_t); ++i) {
        if (!miller_rabin(primes[i]))
            cerr << "fail to detect " << primes[i] << endl;
    }
    size_t large_primes[] = {2147483647, 200560490131, 63018038201, 688846502588399};
    for (size_t i = 0; i < sizeof(large_primes)/sizeof(size_t); ++i) {
        if (!miller_rabin(large_primes[i]))
            cerr << "fail to detect " << large_primes[i] << endl;
    }
    assert(all_of(large_primes,
                  large_primes + sizeof(large_primes)/sizeof(size_t),
                  [] (size_t p) {return miller_rabin(p);}));

    size_t large_composite = 3825123056546413051L;
    cout << large_composite << " is "
         << (miller_rabin(large_composite) ? "prime" : "composite")
         << endl;

    // prime_under

    return 0;
}
