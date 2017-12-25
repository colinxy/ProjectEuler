// pe577


#include <iostream>

using namespace std;

const int MAX = 12345;


int64_t h(int n) {
    int64_t total = 0;

    int64_t side = 1;
    for (int64_t i = n-3; i >= 0; i -= 3) {
        total += (i+2) * (i+1) / 2 * side;
        side++;
    }
    return total;
}

int main() {
    // cout << h(3) << endl;
    // cout << h(6) << endl;
    // cout << h(20) << endl;

    int64_t total = 0;
    for (int i = 3; i <= MAX; i++) {
        total += h(i);
    }
    std::cout << total << std::endl;

    return 0;
}
