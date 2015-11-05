//
// Created by Xinyu Yang on 9/25/15.
//

// #ifndef GRID_H
// #define GRID_H
// 
// #endif // GRID_H

#define N 4

class Grid {
public:
    Grid();

    int grid[N][N];
    int sumCacheRow[N][N];
    int sumCacheCol[N][N];
    void updateSumCache(int, int);
    int rowSum(int);
    int rowSum(int, int);
    int colSum(int);
    int colSum(int, int);
    void printGrid();
    bool check();

    static void nextGrid(int&, int&);
    static void prevGrid(int&, int&);

    int sum;
};
