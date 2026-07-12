% Table Seating with Flexible Groups (and Exact Fit)
%
% Tables must exactly sum to people count in each round.
% Minimizes repeated pairings.

include "globals.mzn";

int: v;             % number of people
int: R;             % number of rounds
int: max_tables;    % maximum tables in any round

% sizes[r,t] = number of people at table t in round r (or 0 if unused)
array[1..R, 1..max_tables] of int: sizes;

% Decision variables
array[1..v, 1..max_tables, 1..R] of var 0..1: x;

% Each person is at exactly one table per round
constraint forall(r in 1..R, i in 1..v)(
  sum(t in 1..max_tables)(x[i, t, r]) = 1
);

% Each table seats exactly the number of people equal to its size
constraint forall(r in 1..R, t in 1..max_tables)(
  sizes[r,t] > 0 ->
    sum(i in 1..v)(x[i, t, r]) = sizes[r,t]
);

% Tables with size 0 must be empty
constraint forall(r in 1..R, t in 1..max_tables)(
  sizes[r,t] = 0 ->
    sum(i in 1..v)(x[i, t, r]) = 0
);

% Track meetings
array[1..v, 1..v] of var 0..R: pair_meet;

constraint forall(i in 1..v, j in i+1..v)(
  pair_meet[i,j] = sum(r in 1..R)(
    sum(t in 1..max_tables)(x[i, t, r] * x[j, t, r])
  )
);

% Minimize repeated pairs
var 0..(v * (v-1) div 2): repeats;

constraint repeats = sum(i in 1..v, j in i+1..v)(max(0, pair_meet[i,j] - 1));

% Symmetry breaking: fix person 1 to table 1 in round 1 (seed)
constraint forall(t in 2..max_tables)(
  x[1, t, 1] = 0
);

solve minimize repeats;

output [
  "v=\(v), rounds=\(R)\n",
  "repeats=\(repeats)\n",
  "x=\(x)\n"
];
