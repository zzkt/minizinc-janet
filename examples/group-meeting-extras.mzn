% Table Seating with Extras & Underfull Tables
%
% Allows fewer seats than people (extras) or more seats than people (underfull).
%
% Primary constraint: minimize extras.
% Secondary constraint: minimize repeated pairs.

include "globals.mzn";

int: v;              % number of people
int: R;              % number of rounds
int: max_tables;     % maximum tables in any round

array[1..R, 1..max_tables] of int: sizes;

% Decision variables
array[1..v, 1..max_tables, 1..R] of var 0..1: x;
array[1..v, 1..R] of var 0..1: extra;

% Each person at exactly one table OR is extra
constraint forall(r in 1..R, i in 1..v)(
  sum(t in 1..max_tables)(x[i, t, r]) + extra[i, r] = 1
);

% Each table has at most its specified size
constraint forall(r in 1..R, t in 1..max_tables)(
  sizes[r,t] > 0 ->
    sum(i in 1..v)(x[i, t, r]) <= sizes[r,t]
);

% Tables with size 0 must be empty
constraint forall(r in 1..R, t in 1..max_tables)(
  sizes[r,t] = 0 ->
    sum(i in 1..v)(x[i, t, r]) = 0
);

% Track pair meetings using linearized constraints
% met[i,j,r] = 1 if person i and j share a table in round r
% Use per-table indicators: same_table[i,j,t,r] = 1 if both at table t
array[1..v, 1..v, 1..max_tables, 1..R] of var 0..1: same_table;

% same_table[i,j,t,r] <= x[i,t,r] and <= x[j,t,r]
constraint forall(i in 1..v, j in i+1..v, t in 1..max_tables, r in 1..R)(
  same_table[i,j,t,r] <= x[i, t, r]
);
constraint forall(i in 1..v, j in i+1..v, t in 1..max_tables, r in 1..R)(
  same_table[i,j,t,r] <= x[j, t, r]
);

% same_table[i,j,t,r] >= x[i,t,r] + x[j,t,r] - 1
constraint forall(i in 1..v, j in i+1..v, t in 1..max_tables, r in 1..R)(
  same_table[i,j,t,r] >= x[i, t, r] + x[j, t, r] - 1
);

% met[i,j,r] = sum over tables of same_table
array[1..v, 1..v, 1..R] of var 0..1: met;
constraint forall(i in 1..v, j in i+1..v, r in 1..R)(
  met[i,j,r] = sum(t in 1..max_tables)(same_table[i,j,t,r])
);

% Total extras
var 0..(v * R): total_extras;
constraint total_extras = sum(i in 1..v, r in 1..R)(extra[i, r]);

% Count repeated pairs (i.e people that met 2 or more times)
var 0..(v * (v-1) div 2): repeats;
constraint repeats = sum(i in 1..v, j in i+1..v)(
  bool2int(sum(r in 1..R)(met[i,j,r]) > 1)
);

% Symmetry breaking
constraint forall(t in 2..max_tables)(
  x[1, t, 1] = 0
);

% Minimize extras, then minimize repeats
solve minimize (total_extras * 1000 + repeats);

output [
  "v=\(v), rounds=\(R)\n",
  "total_extras=\(total_extras)\n",
  "repeats=\(repeats)\n",
  "x=\(x)\n",
  "extra=\(extra)\n"
];
