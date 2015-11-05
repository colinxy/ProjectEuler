#include <iostream>
#include "pe166.h"
using namespace std;


int main() {
    clock_t tStart, tEnd;
    tStart = clock();

    cout << "Executing..." << endl;
    cout << findGridCount() << endl;  // slow, 250s

    tEnd = clock();
    cout << "execution time: " << ((float)tEnd - (float)tStart) / CLOCKS_PER_SEC << endl;
    return 0;
}
