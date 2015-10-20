#include <iostream>
#include <iomanip>
#include "grid.h"
using namespace std;

//
// Created by Xinyu Yang on 9/7/15.
//

Grid::Grid() {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; ++j) {
            grid[i][j] = -1;
        }
    }
}

int
Grid::rowSum(int row) {
    int sum = 0;
    for (int col = 0; col < N; ++col) {
        if (grid[row][col] < 0) throw -1;
        sum += grid[row][col];
    }
    return sum;
}

int
Grid::rowSum(int row, int endCol) {
    int sum = 0;
    for (int col = 0; col <= endCol; ++col) {
        if (grid[row][col] < 0) throw -1;
        sum += grid[row][col];
    }
    return sum;
}

int
Grid::colSum(int col) {
    int sum = 0;
    for (int row = 0; row < N; ++row) {
        if (grid[row][col] < 0) throw -1;
        sum += grid[row][col];
    }
    return sum;
}

int
Grid::colSum(int col, int endRow) {
    int sum = 0;
    for (int row = 0; row <= endRow; ++row) {
        if (grid[row][col] < 0) throw -1;
        sum += grid[row][col];
    }
    return sum;
}

void
Grid::printGrid() {
    for (int row = 0; row < N; ++row) {
        for (int col = 0; col < N; ++col) {
            cout << setw(2) << grid[row][col] << ' ';
        }
        cout << endl;
    }
    cout << endl;
}

void
Grid::nextGrid(int& row, int& col) {
    if (col < N-1) {
        col ++;
    } else {   // col == 3
        row ++;
        col = 0;
    }
}

void
Grid::prevGrid(int &row, int &col) {
    if (col != 0) {
        col --;
    } else {
        row --;
        col = N-1;
    }
}

bool
Grid::check() {
    int sum = Grid::rowSum(0);
    for (int i = 1; i < N; ++i) {
        if (Grid::rowSum(i) != sum) {
            return false;
        }
    }
    for (int i = 0; i < N; ++i) {
        if (Grid::colSum(i) != sum) {
            return false;
        }
    }

    int diagSum = 0, antiDiagSum = 0;
    for (int i = 0; i < N; ++i) {
        diagSum += grid[i][i];
        antiDiagSum += grid[i][N-i-1];
    }

    return !(diagSum != sum || antiDiagSum != sum);
}

void
Grid::updateSumCache(int row, int col) {
    // update column sum
    if (row == 0) {
        sumCacheCol[row][col] = grid[row][col];
    } else {
        sumCacheCol[row][col] = grid[row][col] + sumCacheCol[row-1][col];
    }

    // update row sum
    if (col == 0) {
        sumCacheRow[row][col] = grid[row][col];
    } else {
        sumCacheRow[row][col] = grid[row][col] + sumCacheRow[row][col-1];
    }
}
