// mathutil.cpp


#include <iostream>
#include <cmath>
#include <vector>
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


int64_t Mathutil::pow(int64_t base, int exp) {
    int64_t result = 1;
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


int64_t Mathutil::pow(int64_t base, int exp, int64_t mod) {
    int64_t result = 1;
    while (exp) {
        if (exp & 1) result = result * base % mod;
        exp >>= 1;
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
    for (int i = 1; i <= k; ++i) {
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


bool Mathutil::is_prime(int64_t n) {
    if (n <= 3) return n >= 2;
    if (n % 2 == 0 || n % 3 == 0) return false;

    for (size_t p = 5, limit = (int64_t)sqrt(n)+1; p < limit; p += 6) {
        if (n % p == 0) return false;
        if (n % (p+2) == 0) return false;
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
