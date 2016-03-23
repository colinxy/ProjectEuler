// pe185.cpp

#include <iostream>
#include <vector>
using namespace std;


/*
 * tries to solve the sequence of integers
 * from the guesses made
 */

template <typename T>

class NumberMind {
public:
    NumberMind(const vector<vector<T>> &guesses, const vector<T> &choices)
            : m_row(guesses.size())
            , m_col(guesses[0].size())
            , m_guesses(guesses)
            , m_choice(choices)
            , m_sequence(m_col) {
    }

    void solve() {
        size_t index = 0;
    }

private:
    

    size_t m_row;  // number of guesses
    size_t m_col;  // length of the sequence
    vector<vector<T>> m_guesses;
    vector<T> m_chocies;
    vector<size_t> m_correct;
    vector<size_t> m_current;  // number of correctness currently
    vector<T> m_sequence;
};


int main() {

}
