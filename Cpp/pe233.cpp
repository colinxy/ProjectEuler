// pe233.cpp

/*
 * key: arithmetic progression of square (congruum problem)
 * a^2 + c^2 = 2b^2, a < b < c
 * b^2 - a^2 = c^2 - b^2 = d
 *
 * parametrize difference between squares
 * d = 4mn(m^2 - n^2)
 * b = m^2 + n^2
 *
 * a = -m^2 + n^2 + 2mn
 * c =  m^2 - n^2 + 2mn
 */


#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>

const long TOP = 1000000000L;
const long TOP_SQRT = (long) sqrt(TOP);


long gcd(long a, long b) {
    while (a) {
        long tmp = b;
        b = a;
        a = tmp % a;
    }
    return b;
}


long gcd(long a, long b, long c) {
    return gcd(gcd(a, b), gcd(b, c));
}


void hypotenuses_gen(std::vector<long> &sequence, const long n) {
    // use farey sequence to generate coprime pairs
    long a = 0, b = 1, c = 1, d = n;

    while (c < n) {
        long k = (n + b) / d;
        // a, b are coprime
        long old_c = c, old_d = d;
        c = k*c - a;
        d = k*d - b;
        a = old_c;
        b = old_d;

        // only form hypotenuse when a - b is odd
        if ((b - a) & 1) {
            sequence.push_back(a*a + b*b);
            // if (sequence.size() % 100000000 == 0)
            //     std::cout << sequence.size() << std::endl;
        }
    }
}


int f_even(int N) {
    // N is presumed to be even
    int count = 0;
    int halfSide = N / 2;
    long sq_sum = (long) halfSide * halfSide * 2;

    for (long i = 1; i < halfSide; i++) {
        long other = sq_sum - i*i;
        long sqrt_other = (long) (sqrt(other));
        if (sqrt_other * sqrt_other == other) {
            count++;

            if (gcd(halfSide, i, sqrt_other) == 1)
                std::cout << halfSide << ' ' <<
                    i << ' ' << sqrt_other << std::endl;
        }
    }
    count = count * 8 + 4;

    return count;
}


int main () {
    // std::cout << f_even(10000) << std::endl;

    std::cout << TOP_SQRT << std::endl;
    std::vector<long> hypotenuses;

    hypotenuses_gen(hypotenuses, TOP_SQRT);

    // for (long m = 1; m < TOP_SQRT; m++) {
    //     for (long n = m+1; n < TOP_SQRT; n+=2) {
    //         if (gcd(m, n) == 1) {
    //             hypotenuses.push_back(m*m + n*n);
    //         }
    //     }
    // }

    // std::cout << hypotenuses.size() << std::endl;
    std::sort(hypotenuses.begin(), hypotenuses.end());
    // std::cout << hypotenuses[hypotenuses.size()-1] << std::endl;

    for (int i = 0; i < 100; i++)
        std::cout << hypotenuses[i] << std::endl;

    return 0;
}
