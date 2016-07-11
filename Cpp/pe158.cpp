// pe158.cpp

#include <iostream>
#include <vector>

const int MASK = (1<<26) - 1;

long cache[(1<<26)][2];
// 26 bits to represent whether ch is used
// number of lexiAfter


void nElemSubset(std::vector<int> &subsets, int n, int from) {
    if (n == 0) {
        if (from <= 26) {
            subsets.push_back(0);
        }

        return;
    }

    for (int c = from; c < 26; c++) {
        std::vector<int> sub;
        nElemSubset(sub, n-1, c+1);

        for (size_t i = 0; i < sub.size(); i++) {
            subsets.push_back(sub[i] & (1<<c));
        }
    }
}


void nElemSubset(std::vector<int> &subsets, int n) {
    nElemSubset(subsets, n, 0);
}


int main() {
    for (int k = 0; k < (1<<26); k++) {
        for (int i = 0; i < 2; i++) {
            cache[k][i] = 0;
        }
    }

    for (int i = 0; i < 26; i++) {
        cache[(1<<i) ^ MASK][0] = 1;
    }

    std::vector<int> subsetsN;
    for (int i = 0; i < 26; i++) {
        nElemSubset(subsetsN, i);
        std::cout << i << ' ' << subsetsN.size() << std::endl;
        int count = 0;
        for (size_t j = 0; j < subsetsN.size(); j++)
            count ++;
        subsetsN.clear();
    }

    long max = 0;
    for (int i = 1; i < 26; i++) {
        std::vector<int> subsetsN;
        nElemSubset(subsetsN, i);
        for (int j = 0; j < 26; j++) { // each character

        }
    }

    return 0;
}
