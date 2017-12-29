
#include <iostream>
#include <cmath>
#include <unordered_set>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 18;


struct Frac {
    int32_t numer;
    int32_t denom;

    // Frac() : numer(0), denom(1) {}
    Frac(int numer, int denom=1) : numer(numer), denom(denom) {
        // no longer guarantee minimal representation
        // int div = gcd(numer, denom);
        // this->numer = numer / div;
        // this->denom = denom / div;
    }

    static int gcd(int x, int y) {
        while (y) {
            int tmp = y;
            y = x % y;
            x = tmp;
        }
        return x;
    }

    bool operator==(const Frac &other) const {
        return numer*other.denom == other.numer*denom;
    }
    bool operator<(const Frac &other) const {
        return numer*other.denom < other.numer*denom;
    }
    Frac operator+(const Frac &other) const {
        return Frac(numer*other.denom+denom*other.numer, denom*other.denom);
    }

    void atleast1() {
        if (numer < denom)
            swap(numer, denom);
    }
};

namespace std {
    template <>
    struct hash<Frac> {
        std::size_t operator() (Frac f) const {
            return std::hash<int64_t>()(((int64_t)f.numer<<32) + f.denom);
        }
    };
}


vector<Frac> cache[N+1] = {
    {},
    {{1, 1}},
};

inline Frac parallel(Frac lhs, Frac rhs) {
    return lhs + rhs;
}

inline Frac series(Frac lhs, Frac rhs) {
    swap(lhs.numer, lhs.denom);
    swap(rhs.numer, rhs.denom);
    Frac f = lhs + rhs;
    swap(f.numer, f.denom);
    return f;
}

int d(int n) {
    for (int i = 2; i <= n; i++) {
        for (int n1 = 1; n1 <= i/2; n1++) {
            int n2 = i - n1;

            for (const auto lhs : cache[n1]) {
                for (const auto rhs : cache[n2]) {
                    // symmetry:
                    // pairs of capacitors inverse to each other (except for 1)
                    //
                    // a/b, c/d in series:     (a/b + c/d)
                    // b/a, d/c in parallel: 1/(a/b + c/d)

                    // use symmetry by storing only values >= 1
                    Frac comb1 = parallel(lhs, rhs);
                    comb1.atleast1();
                    cache[i].push_back(comb1);
                    Frac comb2 = series(lhs, rhs);
                    comb2.atleast1();
                    cache[i].push_back(comb2);
                    Frac comb3 = parallel(
                        Frac(lhs.denom, lhs.numer), rhs);
                    comb3.atleast1();
                    cache[i].push_back(comb3);
                    Frac comb4 = series(
                        Frac(lhs.denom, lhs.numer), rhs);
                    comb4.atleast1();
                    cache[i].push_back(comb4);
                }
            }
        }

        sort(cache[i].begin(), cache[i].end());
        auto end = unique(cache[i].begin(), cache[i].end());
        cache[i].erase(end, cache[i].end());
    }

    // unordered_set<Frac> distinct;
    // for (int i = 1; i <= n; i++) {
    //     for (auto d : cache[i])
    //         distinct.insert(d);
    // }

    // numerator and denominator bounded by fibbonacci(n)
    // use loose bound 1<<13
    vector<bool> bitmap(1<<26);
    int distinct = 0;
    for (int i = 1; i <= n; i++) {
        for (auto f : cache[i]) {
            int div = Frac::gcd(f.numer, f.denom);
            f.numer /= div;
            f.denom /= div;
            if (!bitmap[(f.numer<<13) + f.denom]) {
                bitmap[(f.numer<<13) + f.denom] = true;
                distinct++;
            }
        }
    }

    return distinct * 2 - 1;
}

int main() {
    // for (int i = 1; i <= 4; i++)
    //     cout << d(i) << endl;

    cout << d(N) << endl;

    return 0;
}
