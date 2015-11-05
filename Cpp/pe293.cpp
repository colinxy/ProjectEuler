#include <iostream>
#include <cassert>
#include <vector>
#include <set>
#include <algorithm>
#include <cmath>
using namespace std;

const int SIZE = 1000000000;
const int DEGREE = 1000;

void print(vector<int> v) {
    for (size_t i = 0; i < v.size(); ++i) {
        cout << v[i] << endl;
    }
}

bool isPrime(int n) {
    if (n <= 3) return n >= 2;
    if (n % 2 == 0 || n % 3 == 0) return false;

    for (size_t p = 5, limit = (int)sqrt(n)+1; p < limit; p+=6) {
        if (n % p == 0) return false;
        if (n % (p+2) == 0) return false;
    }
    return true;
}

vector<bool> getIsPrimeArray(unsigned long top) {
    assert(sizeof(size_t) == 8);
    vector<bool> isPrime(top, true);
    
    for (size_t i = 2; i < top; ++i) {
        if (isPrime[i]) {
            // assume sizeof(size_t) == 8
            for (size_t j = i*i; j < top; j+=i) {
                isPrime[j] = false;
            }
        }
    }
    
    return isPrime;
}
/*
void admissibleGen(vector<int> & admissible, int primeIndex,
                   vector<int> & primes, size_t startIndex) {

    if (primeIndex == primes.size()) return;

    vector<int> toAdd;
    int prime = primes[primeIndex];
    for (size_t i = startIndex; i < admissible.size(); i++) {
        int64_t current = admissible[i];
        while ((current *= prime) < SIZE) {
            cout << current << " " << prime << endl;
            toAdd.push_back((int) current);
        }
    }

    startIndex = admissible.size();

    for (auto num : toAdd) {
        admissible.push_back(num);
    }


    admissibleGen(admissible, primeIndex+1, primes, startIndex);
}*/

int main() {
    int64_t primeProduct = 1;
    vector<int> primes;
    vector<bool> isPrimeArray = getIsPrimeArray(SIZE / DEGREE);
    for (size_t i = 2; i < SIZE; ++i) {
        if (isPrimeArray[i]) {
            primeProduct *= i;
            if (primeProduct > SIZE) break;

            primes.push_back((int)i);
        }
    }
    // print(primes);
    
    vector<int> admissible;
    for (int i = 2; i < SIZE; i *= 2) {
        admissible.push_back(i);
    }

    size_t startIndex = 0;
    for (size_t primeIndex = 0; primeIndex < primes.size(); ++primeIndex) {
        vector<int> toAdd;
        int prime = primes[primeIndex];
        for (size_t i = startIndex; i < admissible.size(); ++i) {
            int64_t current = admissible[i];
            while ((current *= prime) < SIZE) {
                // cout << current << " " << prime << endl;
                toAdd.push_back((int) current);
            }
        }

        startIndex = admissible.size();

        for (auto num : toAdd) {
            admissible.push_back(num);
        }
    }
    // cout << admissible.size() << endl;

    sort(admissible.begin(), admissible.end());
    // cout << admissible[0] <<  ' ' << admissible[admissible.size()-1] << endl;

    set<int> pfNum;
    size_t admIndex = 0;
    size_t currentPrime;

    while (admIndex < admissible.size()) {
        int num = admissible[admIndex];
        for (int incre = 2; ; ++incre) {
            if (isPrime(num+incre)) {
                currentPrime = num + incre;
                pfNum.insert(incre);
                // cout << incre << ' ' << num << endl;
                break;
            }
        }
        ++admIndex;
        while (admIndex < admissible.size()) {
            num = admissible[admIndex];
            if (num < currentPrime-1) {
                pfNum.insert(currentPrime - num);
                // cout << currentPrime - num << ' ' << num << endl;
                ++admIndex;
            } else {
                break;
            }
        }
    }

    int64_t total = 0;
    for (int num : pfNum) {
        total += num;
    }

    cout << total << endl;

    return 0;
}
