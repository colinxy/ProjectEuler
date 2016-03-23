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
    cout << "testing is_prime" << endl;
    size_t primes[] = {37, 101, 947, 7873, 2147483647};
    for (size_t p : primes) {
        if (! is_prime(p))
            cerr << "fail to detect " << p << endl;
    }

    // miller_rabin
    cout << "testing miller rabin" << endl;
    for (size_t i = 0; i < sizeof(primes)/sizeof(size_t); ++i) {
        if (!miller_rabin(primes[i]))
            cerr << "fail to detect " << primes[i] << endl;
    }
    size_t large_primes[] = {2147483647L, 200560490131L, 63018038201L, 688846502588399L};
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
