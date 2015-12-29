// mathutil.h


#ifndef MATHUTIL_H
#define MATHUTIL_H


namespace Mathutil {
      // functions for arithmetic utility
    int64_t sum(int64_t, int64_t, int64_t);
    int64_t pow(int64_t, int);
    int64_t pow(int64_t, int, int64_t);

      // functions for dealing with prime numbers
    bool   is_prime(int64_t n);
    size_t prime_under(vector<int> &primes, size_t n);
}

#endif  // MATHUTIL_H
