% n-Queens problem
% Place n queens on an nxn board so no two attack each other.

int: n;
array [1..n] of var 1..n: q;

include "alldifferent.mzn";

constraint alldifferent(q);
constraint alldifferent(i in 1..n)(q[i] + i);
constraint alldifferent(i in 1..n)(q[i] - i);

solve satisfy;
