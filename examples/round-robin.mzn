% All Pairs in a Group Meeting Once.
%
% Round-robin pairing where each pair meets exactly once.
% For odd n, one person sits out each round.
% Uses symmetry breaking + exact coverage constraints.
%
% Note: the polygon/circle algorithm will almost always
% outperform the constraint solver for finding pairs.

include "globals.mzn";

int: v;    % number of people
int: R;    % number of rounds (= v for odd, v-1 for even)

% Decision variables: x[i,j,r] = 1 if person i paired with person j in round r
array[1..v, 1..v, 1..R] of var 0..1: x;

% Symmetry: x[i,j,r] = x[j,i,r]
constraint forall(r in 1..R, i in 1..v, j in i+1..v)(
  x[i, j, r] = x[j, i, r]
);

% No self-pairs
constraint forall(r in 1..R, i in 1..v)(
  x[i, i, r] = 0
);

% Each person in exactly one pair per round (or sits out if odd)
constraint forall(r in 1..R, i in 1..v)(
  sum(j in 1..v)(x[i, j, r]) <= 1
);

% Each pair meets at most once
constraint forall(i in 1..v, j in i+1..v)(
  sum(r in 1..R)(x[i, j, r]) <= 1
);

% Cover all pairs exactly once
constraint sum(r in 1..R, i in 1..v, j in i+1..v)(x[i, j, r]) = (v * (v-1)) div 2;

% Symmetry breaking: person 1 paired with person 2 in round 1
constraint x[1, 2, 1] = 1;

solve satisfy;

output [
  "v=\(v), rounds=\(R)\n",
  "x=\(x)\n"
];
