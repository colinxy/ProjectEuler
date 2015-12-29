// mathutil.h


#include <iostream>
#include <vector>
using namespace std;


/*
 * functions for arithmetic utility
 */
int64_t pow(int, int);
int64_t pow(int, int, int);


/*
 * functions for dealing with prime numbers
 */
bool   is_prime(long n);
size_t prime_under(vector<int> &primes, size_t n);
