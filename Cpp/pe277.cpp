// pe277.cpp

#include <iostream>
#include <cstring>

const char *SEQ = "UDDDUdddDDUDDddDdDddDDUDDdUUDd";
const long N = 1000000000000000; // 10^15

int main() {
    std::cout << "algorithm too slow for problem size, \n"
        "see python solution" << std::endl;
    std::size_t length = strlen(SEQ);

    for (long num = N; ; ++num) {
        if (num % 1000000000 == 0)
            std::cout << num << std::endl;

        bool match = true;
        long curr = num;
        for (std::size_t i = 0; i < length; ++i) {
            char step;
            if (curr % 3 == 0) {
                step = 'D';
                curr /= 3;
            } else if (curr % 3 == 1) {
                step = 'U';
                curr = (4*curr+2) / 3;
            } else {
                step = 'u';
                curr = (2*curr-1) / 3;
            }

            if (step != SEQ[i]) {
                match = false;
                break;
            }
        }

        if (match) {
            std::cout << num << std::endl;
            break;
        }
    }

    return 0;
}
