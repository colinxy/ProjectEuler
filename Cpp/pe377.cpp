#include <iostream>
#include <utility>
using namespace std;

const int DIGIT = 9;
const int MOD = 1000000000;

/*
int64_t total = 0;
const int64_t powers[] = {
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
    100000000LL, 1000000000LL, 10000000000LL, 100000000000LL,
    1000000000000LL, 10000000000000LL, 100000000000000LL
};

int choices(int left, int nth_digit) {
    if (left == 0) {
        return 1;
    }

    int numOfChoice = 0;
    for (int i = 1; i < 10; ++i) {
        if (left < i) break;

        int currChoice = choices(left - i, nth_digit+1);
        total += i * powers[nth_digit] * currChoice;
        numOfChoice += currChoice;
    }

    return numOfChoice;
}
*/

// returns a pair, (numOfChoice, totalSum)
pair<int64_t, int64_t> choicesMod(int left, int nth_digit) {
    if (left == 0) {
        return make_pair(1, 0);
    }

    int64_t numOfChoice = 0;
    int64_t totalSum = 0;
    for (int i = 1; i < 10; ++i) {
        if (left < i) break;

        pair<int64_t, int64_t> currChoice = choicesMod(left - i, nth_digit+1);
        numOfChoice = (numOfChoice + currChoice.first) % MOD;
        if (nth_digit <= DIGIT) {
            totalSum = (totalSum + i*currChoice.first + currChoice.second*10) % MOD;
        }
    }

    return make_pair(numOfChoice, totalSum);
}

int main() {
    // cout << choices(13, 0) << endl;
    // cout << total << endl;

    pair<int64_t, int64_t> choiceAndSum = choicesMod(30, 0);
    cout << choiceAndSum.first << endl;
    cout << choiceAndSum.second << endl;

    return 0;
}
