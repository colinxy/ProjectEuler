#include <iostream>
#include "pe166.h"
using namespace std;

//
// Created by Xinyu Yang on 9/13/15.
// the value for N in grid.h is customizable,
// as long as the N >= 3
//

int gridCount = 0;

void
fillGrid(Grid& grid, int currRow, int currCol) {
    if (currRow == 1 && currCol == 0) {  // first row finished, get the sum
        grid.sum = grid.rowSum(0);
    }

    if (currRow == N-2 && currCol == N-1) {  // the grid is determined
        int left;

        // (N-2, N-1)
        int currRowSum = grid.rowSum(currRow, currCol-1);
        left = grid.sum - currRowSum;
        if (left >= 0 && left <= 9) {
            grid.grid[currRow][currCol] = left;
        } else {
            // backtrack
            grid.grid[currRow][currCol] = -1;
            return;
        }

        // last row
        for (int col = 0; col < N; ++col) {
            int colSum = grid.colSum(col, N-2);
            left = grid.sum - colSum;
            if (left >= 0 && left <= 9) {
                grid.grid[N-1][col] = left;
            } else {
                // backtrack
                grid.grid[currRow][currCol] = -1;
                return;
            }
        }

        if (grid.check()) {
            gridCount++;
            // cout << gridCount << endl;
            // grid.printGrid();
        }

    } else if (currRow > 0 && currCol == N-1) {  // last column of the row
        int currRowSum = grid.rowSum(currRow, N-2);

        int left = grid.sum - currRowSum;
        if (left >= 0 && left <= 9) {
            grid.grid[currRow][currCol] = left;
        } else {
            // backtrack
            grid.grid[currRow][currCol] = -1;
            return;
        }
        Grid::nextGrid(currRow, currCol);  // update the current grid
        fillGrid(grid, currRow, currCol);

    } else {
        for (int i = 0; i < 10; ++i) {

            grid.grid[currRow][currCol] = i;
            grid.updateSumCache(currRow, currCol);

            if (currRow > 0 && currCol < N - 1) {
                // early terminate row sum
                if (grid.sumCacheRow[currRow][currCol] > grid.sum) {
                    // backtrack
                    grid.grid[currRow][currCol] = -1;
                    return;
                }

                // early terminate column sum
                if (grid.sumCacheCol[currRow][currCol] > grid.sum) {
                    // backtrack
                    grid.grid[currRow][currCol] = -1;
                    return;
                }
            }
            Grid::nextGrid(currRow, currCol);  // update the current grid
            fillGrid(grid, currRow, currCol);
            Grid::prevGrid(currRow, currCol);
        }
    }

}

int
findGridCount() {
    Grid grid = Grid();

    // grid.printGrid();

    int currRow = 0;
    int currCol = 0;

    fillGrid(grid, currRow, currCol);

    return gridCount;
}
