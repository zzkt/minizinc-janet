% Sudoku puzzle solver
% Fill a 9x9 grid so each row, column, and 3x3 box contains 1-9.

int: n = 9;
array[1..n, 1..n] of var 1..n: x;

include "alldifferent.mzn";

% Rows
constraint forall(i in 1..n)(all_different(row(x, i)));

% Columns
constraint forall(j in 1..n)(all_different(col(x, j)));

% 3x3 boxes
constraint forall(bi in 0..2, bj in 0..2)(
  all_different([x[bi*3+i, bj*3+j] | i in 1..3, j in 1..3])
);

solve satisfy;
