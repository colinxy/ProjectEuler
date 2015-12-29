// mathutil.cpp


#include <iostream>
#include <cmath>
#include <vector>
#include "mathutil.h"
using namespace std;


/*
 * no integer overflow is checked!!!
 */
// int64_t pow(int base, int exp) {}


bool is_prime(long n) {
    if (n <= 3) return n >= 2;
    if (n % 2 == 0 || n % 3 == 0) return false;

    for (size_t p = 5, limit = (int64_t)sqrt(n)+1; p < limit; p += 6) {
        if (n % p == 0) return false;
        if (n % (p+2) == 0) return false;
    }
    return true;
}

/*
 * primes: empty integer vector with enough space resevered (preferred)
 */
size_t prime_under(vector<int> &primes, size_t n) {
    size_t size = 0;
    vector<bool> isPrime(n, true);

    for (int prime = 2; prime < n; ++prime) {
        if (isPrime[prime]) {
            primes.push_back(prime);
            ++size;

            for (int64_t next = (int64_t)prime * prime; next < n; next += prime)
                isPrime[next] = false;
        }
    }

    return size;
}
