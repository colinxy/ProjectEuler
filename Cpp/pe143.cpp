// pe143.cpp

#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_set>

using std::vector;
using std::unordered_set;

struct Pair {
    int p;
    int q;

    Pair(int p, int q) : p(p), q(q) {}
};

bool operator==(const Pair &lhs, const Pair &rhs) {
    return lhs.p == rhs.p && lhs.q == rhs.q;
}

namespace std {
    template<> struct hash<Pair> {
        std::size_t operator()(const Pair& pair) const {
            return std::hash<int>()(pair.p) ^ std::hash<int>()(pair.q);
        }
    };
}

bool is_square(long n) {
    long n_sqrt = (long) (sqrt(n)+0.5);
    return n_sqrt * n_sqrt == n;
}

void all_pairs(vector<Pair> &pairs, int bound) {
    // find p, q such that p^2 + q^2 + p*q = a^2, where a is integer
    for (int p = 1; p < bound/2; ++p) {
        long p_sq = (long)p * p;
        for (int q = p+1; q < bound-p; q++) {
            long candidate = p_sq + q * (p+q);
            if (is_square(candidate))
                pairs.push_back(Pair(p, q));
        }
    }
}


int main() {
    vector<Pair> pairs;
    all_pairs(pairs, 120000);
    std::cout << "total: " << pairs.size() << std::endl;

    unordered_set<Pair> lookup_set(pairs.cbegin(), pairs.cend());
    unordered_set<int> distinct_pqr;

    std::size_t size = pairs.size();
    for (std::size_t i = 0; i < size; ++i) {
        int p = pairs[i].p;
        int q = pairs[i].q;
        for (std::size_t j = i+1; j < size; ++j) {
            if (p != pairs[j].p)
                break;
            if (lookup_set.find(Pair(q, pairs[j].q)) != lookup_set.end()) {
                long sum = p + q + pairs[j].q;
                std::cout << sum << std::endl;
                distinct_pqr.insert(sum);
            }
        }
    }

    long distinct_sum = 0;
    for (auto iter = distinct_pqr.cbegin();
         iter != distinct_pqr.cend(); ++iter) {
        distinct_sum += *iter;
    }

    std::cout << distinct_sum << std::endl;

    return 0;
}
