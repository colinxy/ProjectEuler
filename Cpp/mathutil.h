// mathutil.h


#ifndef MATHUTIL_H
#define MATHUTIL_H

#include <iostream>
#include <vector>


namespace Mathutil {
      // arithmetic utility
    int64_t sum(int64_t, int64_t, int64_t);
    size_t  pow(size_t, size_t);
    size_t  pow(size_t, size_t, size_t);
    size_t  pow_s(size_t, size_t, size_t);

    int64_t gcd(int64_t, int64_t);

      // combinatoric
    int64_t  nCr(int, int);
    uint64_t nCr_s(unsigned int, unsigned int);

      // dealing with prime numbers
    bool   is_prime(size_t);
    bool   miller_rabin(size_t);
    size_t prime_under(std::vector<int> &primes, size_t n);
}

#endif  // MATHUTIL_H
