// mathutil.cpp


#include <iostream>
#include <algorithm>
#include <cmath>
#include <array>
#include <vector>
#include <utility>
#include <stdexcept>
#include "mathutil.h"
using namespace std;


/*
 * arithmetic utility
 */


int64_t Mathutil::sum(int64_t from, int64_t to, int64_t diff) {
    if (to < from) return 0;

    int64_t size = (to - from) / diff + 1;
    to = from + (size - 1) * diff;

    if ((from + to) % 2 == 0)
        return (from + to) / 2 * size;
    return size / 2 * (from + to);
}


size_t Mathutil::pow(size_t base, size_t exp) {
    size_t result = 1;
    while (exp) {
        if (exp & 1) result *= base;
        exp >>= 1;
        result *= result;
    }
    return result;

    /*
    if (exp == 0)
        return 1;

    if (exp % 2 == 0)
        return pow(base*base, exp/2);

    else
        return pow(base*base, exp/2) * base;
    */
}


size_t Mathutil::pow(size_t base, size_t exp, size_t mod) {
    size_t result = 1;
    base %= mod;
    while (exp) {
        if (exp & 1) result = result * base % mod;
        exp >>= 1;
        result = result * result % mod;
    }
    return result;
}


size_t pow_mod(size_t base, size_t exp, size_t mod) {
    size_t result = 1;
    base %= mod;
    for (size_t i = 0; i < exp; ++i)
        result = result * base % mod;

    return result;
}


// less possible to overflow
// revert to
size_t Mathutil::pow_s(size_t base, size_t exp, size_t mod) {
    size_t result = 1;
    base %= mod;
    while (exp) {
        if (exp & 1) {
            if (result > numeric_limits<size_t>::max() / base) {
                // throw overflow_error("overflow in computing modular exponentiation");
                cout << "overflow for exponent algorithm, using product algorithm" << endl;
                return pow_mod(base, exp, mod);
            }
            result = result * base % mod;
        }

        exp >>= 1;

        if (result > numeric_limits<size_t>::max() / result) {
            // throw overflow_error("overflow in computing modular exponentiation");
            return pow_mod(base, exp, mod);
        }
        result = result * result % mod;
    }
    return result;
}


// https://en.wikipedia.org/wiki/Binary_GCD_algorithm
int64_t Mathutil::gcd(int64_t u, int64_t v) {
    int shift;

    if (u == 0) return v;
    if (v == 0) return u;

    for (shift = 0; ((u | v) & 1) == 0; ++shift) {
        u >>= 1;
        v >>= 1;
    }

    while ((u & 1) == 0)
        u >>= 1;

    // from here on, u is always odd
    do {
        while ((v & 1) == 0)
            v >>= 1;

        if (u > v)
            swap(u, v);

        v -= u;
    } while (v != 0);

    return u << shift;
}


/*
 * combinatoric
 */


// possible overflow
int64_t Mathutil::nCr(int n, int k) {
    if (n < k) return 0;

    if (k > n / 2) k = n - k;

    int64_t result = 1;
    for (int i = 1; i <= k; ++i) {
        result *= n--;
        result /= i;
    }
    return result;
}


// overflow safe
uint64_t Mathutil::nCr_s(unsigned int n, unsigned int k) {
    if (k > n) return 0;

    if (k > n / 2) k = n - k;

    uint64_t result = 1;
    for (unsigned int i = 1; i <= k; ++i) {
        uint64_t g = gcd(result, i);
        result /= g;
        uint64_t t = n-- / (i / g);

        if (result > numeric_limits<uint64_t>::max() / t) {
            throw overflow_error("overflow in computing combination");
        }

        result *= t;
    }

    return result;
}


/*
 * dealing with prime numbers
 */


bool Mathutil::is_prime(const size_t n) {
    if (n <= 3) return n >= 2;
    if (!(n & 1) || n % 3 == 0) return false;

    for (size_t p = 5, limit = (size_t)sqrt(n)+1; p < limit; p += 6) {
        if (n % p == 0) return false;
        if (n % (p+2) == 0) return false;
    }
    return true;
}


/*
 * n-1 = 2^s * d
 *
 * a^d % n == 1  OR
 * a^(2^r*d) % n == -1   0 <= r <= s-1
 */

bool witness(size_t /*n*/, size_t /*a*/, size_t /*s*/, size_t /*d*/) {
    // size_t rem = Mathutil::pow_s(a, d, n);
    // if (rem == 1) return true;  // probable prime

    // for (size_t i = 0; i < s; ++i) {
    //     rem = rem * rem % n;

    //     if (rem == 1)   return false;
    //     if (rem == n-1) return true;  // probable prime
    // }

    return false;
}

/*
 * if n < 1,373,653, it is enough to test a = 2 and 3;
 * if n < 9,080,191, it is enough to test a = 31 and 73;
 * if n < 4,759,123,141, it is enough to test a = 2, 7, and 61;
 * if n < 1,122,004,669,633, it is enough to test a = 2, 13, 23, and 1662803;
 * if n < 2,152,302,898,747, it is enough to test a = 2, 3, 5, 7, and 11;
 * if n < 3,474,749,660,383, it is enough to test a = 2, 3, 5, 7, 11, and 13;
 * if n < 341,550,071,728,321, it is enough to test a = 2, 3, 5, 7, 11, 13, and 17.
 *
 * if n < 18,446,744,073,709,551,616 = 2^64,
 * it is enough to test a = 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, and 37
 */

bool Mathutil::miller_rabin(size_t n) {
    if (n <= 3) return n >= 2;
    if (!(n & 1) || n % 3 == 0) return false;

    size_t s = 0;
    size_t d = n-1;

    while (!(d & 1)) {
        d /= 2;
        ++s;
    }

    array<size_t, 12> small_primes {{
            2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37}};

    for (auto p : small_primes) {
        if (!witness(n, p, s, d))
            return false;
    }

    return true;
}


/*
 * @param
 * primes: empty integer vector with enough space resevered (preferred)
 */
size_t Mathutil::prime_under(vector<int> &primes, size_t n) {
    size_t size = 0;
    vector<bool> isPrime(n, true);

    for (unsigned int prime = 2; prime < n; ++prime) {
        if (isPrime[prime]) {
            primes.push_back(prime);
            ++size;

            for (uint64_t next = (uint64_t)prime * prime; next < n; next += prime)
                isPrime[next] = false;
        }
    }

    return size;
}
