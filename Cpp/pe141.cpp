// pe141.cpp

/*
 * something to write here
 *
 * To be honest, this problem is easy, definitely not worth the 60%
 * it is labeled with. But I was unable to find a fast enough solution
 * for this problem. It is really disappointing to look at other
 * people's solution when you know you can solve the problem yourself.
 * All the technique used in this problem is basic, even straightforward.
 * There have been plenty of problems like this where I felt like I could
 * solve it easily, but I miserably failed. Even though I have intended to
 * use project euler as a practice site, but repeatedly looking at other
 * people's solution has defeated the purpose.
 *
 * The parametrization technique used in this problem helps to bring
 * down the complexity by a huge amount, surprisingly.
 *
 */

#include <iostream>
#include <cmath>


bool is_square(long n) {
    long sqrt_n = floor(sqrt(n));
    return sqrt_n * sqrt_n == n;
}

long gcd(long a, long b) {
    return a == 0 ? b : gcd(b%a, a);
}

void parametrized();

// problem size 10^12
const long N = 10000000000L;

int main() {
    parametrized();

    // long progress_sum = 0;
    // for (long d = 2; d < 1000000; d++) {
    //     for (long r = d; r >= 1; r--) {
    //         long d_cubed = d * d * d;

    //         // INCORRECT!!!, d^2 also has to divide r
    //         if (d_cubed % r != 0)
    //             continue;

    //         long candidate = d_cubed / r + r;
    //         if (candidate > N)
    //             break;

    //         if (is_square(candidate)) {
    //             progress_sum += candidate;
    //             std::cout << candidate << ' ' << d << ' ' << r << std::endl;
    //         }
    //     }
    // }

    // std::cout << progress_sum << std::endl;

    return 0;
}

// http://www.mathblog.dk/project-euler-141investigating-progressive-numbers-n-which-are-also-square/
void parametrized() {
    long sum = 0;
    for (long a = 2; a < 10000; a++) {

        for (long b = 1; b < a; b++) {
            if (a*a*a * b >= N) break;
            if (gcd(b, a) != 1) continue;

            for (long c = 1; ; c++) {
                long n = a*a*a * b * c*c + c * b*b;
                if (n >= N) break;

                if (is_square(n)) {
                    sum += n;
                    std::cout << n << std::endl;
                }

            }
        }
    }

    std::cout << sum << std::endl;
}
