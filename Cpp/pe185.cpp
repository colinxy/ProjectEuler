// pe185.cpp

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <utility>
#include <unordered_map>
using namespace std;


/*
 * tries to solve the sequence of integers
 * from the guesses
 */

const vector<vector<int> > guesses {
    {9, 0, 3, 4, 2},
    {7, 0, 7, 9, 4},
    {3, 9, 4, 5, 8},
    {3, 4, 1, 0, 9},
    {5, 1, 5, 4, 5},
    {1, 2, 5, 3, 1}
};
const vector<size_t> numCorrect {2, 0, 2, 1, 2, 1};

// reordered in ascending order
// different in sequence from the problem
const vector<vector<int> > guessesBig {
    {2, 3, 2, 1, 3, 8, 6, 1, 0, 4, 3, 0, 3, 8, 4, 5},
    {3, 1, 7, 4, 2, 4, 8, 4, 3, 9, 4, 6, 5, 8, 5, 8},
    {3, 8, 4, 7, 4, 3, 9, 6, 4, 7, 2, 9, 3, 0, 4, 7},
    {4, 8, 9, 5, 7, 2, 2, 6, 5, 2, 1, 9, 0, 3, 0, 6},
    {6, 3, 7, 5, 7, 1, 1, 9, 1, 5, 0, 7, 7, 0, 5, 0},
    {6, 9, 1, 3, 8, 5, 9, 1, 7, 3, 1, 2, 1, 3, 6, 0},
    {8, 1, 5, 7, 3, 5, 6, 3, 4, 4, 1, 1, 8, 4, 8, 3},
    {2, 3, 2, 6, 5, 0, 9, 4, 7, 1, 2, 7, 1, 4, 4, 8},
    {2, 6, 1, 5, 2, 5, 0, 7, 4, 4, 3, 8, 6, 8, 9, 9},
    {2, 6, 5, 9, 8, 6, 2, 6, 3, 7, 3, 1, 6, 8, 6, 7},
    {4, 5, 1, 3, 5, 5, 9, 0, 9, 4, 1, 4, 6, 1, 1, 7},
    {5, 2, 5, 1, 5, 8, 3, 3, 7, 9, 6, 4, 4, 3, 2, 2},
    {5, 6, 1, 6, 1, 8, 5, 6, 5, 0, 5, 1, 8, 2, 9, 3},
    {6, 4, 4, 2, 8, 8, 9, 0, 5, 5, 0, 4, 2, 7, 6, 8},
    {1, 7, 4, 8, 2, 7, 0, 4, 7, 6, 7, 5, 8, 2, 7, 6},
    {1, 8, 4, 1, 2, 3, 6, 4, 5, 4, 3, 2, 4, 5, 8, 9},
    {3, 0, 4, 1, 6, 3, 1, 1, 1, 7, 2, 2, 4, 6, 3, 5},
    {4, 2, 9, 6, 8, 4, 9, 6, 4, 3, 6, 0, 7, 5, 4, 3},
    {5, 8, 5, 5, 4, 6, 2, 9, 4, 0, 8, 1, 0, 5, 8, 7},
    {7, 8, 9, 0, 9, 7, 1, 5, 4, 8, 9, 0, 8, 0, 6, 7},
    {8, 6, 9, 0, 0, 9, 5, 8, 5, 1, 5, 2, 6, 2, 5, 4},
    {9, 7, 4, 2, 8, 5, 5, 5, 0, 7, 0, 6, 8, 3, 5, 3}
};
const vector<size_t> numCorrectBig {0, 1, 1, 1, 1, 1, 1, 2, 2, 2,
        2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3};

const size_t NUM_GUESSES = 22;
const size_t LENGTH = 16;


struct Guess {
    Guess() : correct(0) {
        for (size_t i = 0; i < LENGTH; ++i)
            guess[i] = false;
    }

    bool operator[] (size_t index) const {
        return guess[index];
    }

    size_t numCorrect() const {
        return correct;
    }

    void setCorrect(size_t index) {
        guess[index] = true;
        ++correct;
    }

    void setIncorrect(size_t index) {
        if (!guess[index])
            return;

        guess[index] = false;
        --correct;
    }

private:
    // bool check() const {
    //     size_t count = 0;
    //     for (size_t i = 0; i < LENGTH; ++i)
    //         if (guess[i])
    //             ++count;
    //     return correct == count;
    // }

    size_t correct;
    bool guess[LENGTH];
};


class NumberMind {
public:
    NumberMind(const vector<vector<int> > &guesses,
               const vector<size_t> &numCorrect)
        : m_guesses(guesses)
        , m_numCorrect(numCorrect)
        , m_result(LENGTH, -1) {
    }

    // solve by column
    vector<int> solve() {
        solutionFound = false;
        // second half
        solveSub(LENGTH/2, LENGTH, true);
        cout << "cache computed" << endl;
        // first half
        solveSub(0, LENGTH/2, false);

        if (!solutionFound)
            cerr << "No solution" << endl;

        return m_result;
    }

private:
    // solve a subproblem
    // from <= index < end
    void solveSub(size_t begin, size_t end, bool fillCache) {
        vector<Guess> correctness(NUM_GUESSES);

        size_t index = begin;      // current index in the result sequence
        while (index < end) {
            if (m_result[index] == -1) {
                m_result[index] = 0;
            } else if (m_result[index] == 9) {
                if (index == begin)
                    break;

                m_result[index] = -1;
                setIncorrect(index, correctness, NUM_GUESSES);
                index--;
                continue;
            } else {
                setIncorrect(index, correctness, NUM_GUESSES);
                m_result[index]++;
            }

            if (index == begin) {
                for (size_t i = begin; i < end; ++i)
                    cout << m_result[i];
                cout << endl;
            }
            // cout << "ind: " << index << " val: " << m_result[index] << endl;

            // propagate constraint
            bool satisfactory = true;
            for (size_t i = 0; i < NUM_GUESSES; ++i) {
                if (m_guesses[i][index] == m_result[index]) {
                    correctness[i].setCorrect(index);

                    if (correctness[i].numCorrect() > m_numCorrect[i]) {
                        setIncorrect(index, correctness, i+1);
                        satisfactory = false;
                        break;
                    }
                }
            }
            if (!satisfactory)
                continue;

            // cout << endl;
            // for (size_t i = 0; i < LENGTH; ++i)
            //     cout << m_result[i];
            // cout << endl;

            if (index == end-1) {
                // for second half of the problem
                if (fillCache) {
                    vector<size_t> remainNumCorrect(m_numCorrect);
                    for (size_t i = 0; i < NUM_GUESSES; ++i) {
                        remainNumCorrect[i] -= correctness[i].numCorrect();
                    }
                    vector<int> result(m_result.begin() + begin,
                                       m_result.begin() + end);

                    pair<string, vector<int> > pr(
                        vec2string(remainNumCorrect), result);
                    m_secondHalfCache.insert(pr);
                }
                // for first half of the problem
                else {
                    vector<size_t> numCorrect;
                    for (size_t i = 0; i < NUM_GUESSES; ++i) {
                        numCorrect.push_back(correctness[i].numCorrect());
                    }
                    auto got = m_secondHalfCache.
                        find(vec2string(numCorrect));

                    if (got != m_secondHalfCache.end()) {
                        solutionFound = true;

                        const vector<int> &otherHalf = got->second;

                        // copy into result
                        size_t j = 0;
                        for (size_t i = LENGTH/2; i < LENGTH; ++i)
                            m_result[i] = otherHalf[j++];
                        return;
                    }
                }
                continue;
            }

            index++;
        }

        // for (size_t i = 0; i < NUM_GUESSES; ++i) {
        //     for (size_t j = 0; j < LENGTH; ++j)
        //         cout << correctness[i][j];
        //     cout << endl;
        //     cout << correctness[i].numCorrect() << endl;
        // }
    }

    static void setIncorrect(size_t index, vector<Guess> &correctness,
        size_t before) {

        for (size_t i = 0; i < before; ++i) {
            correctness[i].setIncorrect(index);
        }
    }

    string vec2string(const vector<size_t> &vec) {
        stringstream ss;
        for (size_t i : vec)
            ss << i;
        return ss.str();
    }

    const vector<vector<int> > m_guesses;
    const vector<size_t> m_numCorrect;

    unordered_map<string, vector<int> > m_secondHalfCache;
    vector<int> m_result;
    bool solutionFound;
};


int main() {
    vector<int> result = NumberMind(guessesBig, numCorrectBig).solve();
    for (size_t i : result)
        cout << i;
    cout << endl;

    return 0;
}
