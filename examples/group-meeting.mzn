% Table Seating Problem as Resolvable Block Design
%
% The table seating problem asks: given v people and tables of capacity k,
% how many rounds are needed so that every pair of people shares a table
% exactly once?
%
% This is equivalent to finding a RESOLVABLE (v, k, 1)-DESIGN
%   - v: number of points (people)
%   - k: block size (table capacity)
%   - λ=1: each pair appears in exactly one block
%   - Resolvable: blocks can be partitioned into PARALLEL CLASSES
%                 (rounds), where each class covers all v points


% EXISTENCE CONDITIONS
%
% For a resolvable (v, k, 1)-design to exist:
%
%   1. v ≡ 0 (mod k)        — people divide evenly into tables
%   2. v-1 ≡ 0 (mod k-1)    — each person meets k-1 others per round
%
% Number of rounds needed:  r = (v-1) / (k-1)
% Number of tables per round: v/k
% Total pairs covered:  r × (v/k) × C(k,2) = v(v-1)/2 = C(v,2) ✓

% EXAMPLES
%
%   k=2 (pairs):  r = v-1 rounds. Our round-robin polygon method!
%                  Exists for all even v ≥ 4.
%
%   k=3:  Resolvable (v,3,1)-designs exist ⟺ v ≡ 3 (mod 6)
%          Kirkman's Schoolgirl Problem: v=15, k=3, r=7
%
%   k=4:  Exist for v ≡ 0 (mod 4) with some exceptions.
%          The "Dagstuhl" problem: v=20, k=4, r=19/3 ≈ not integer
%          so strict version doesn't exist — use "at most once" instead.

% INTEGER PROGRAMMING FORMULATION
%
% Use a binary matrix x[i,j,r] where:
%   x[i,j,r] = 1  iff  person i sits at table j in round r
%
% Constraints:
%   (C1) Each person at exactly one table per round:
%        ∀i,r: Σ_j x[i,j,r] = 1
%
%   (C2) Each table has exactly k people (or ≤ k for flexible):
%        ∀j,r: Σ_i x[i,j,r] = k
%
%   (C3) Each pair shares a table at most once:
%        ∀i<j, ∀j1<j2: Σ_r (x[i,j1,r]×x[j,j1,r] + ...) ≤ 1
%        (linearized: for each pair (i,j), count co-assignments ≤ 1)
%
%   (C4) Resolvability: tables in each round are disjoint
%        (ensured by C1 + C2)

include "globals.mzn";

% Parameters
int: v;                           % number of people
int: k;                           % table capacity
int: r = (v - 1) div (k - 1);     % number of rounds (derived)
int: tables_per_round = v div k;  % tables in each round

% Sanity checks
constraint assert(v mod k = 0, "v must be divisible by k");
constraint assert((v - 1) mod (k - 1) = 0, "v-1 must be divisible by k-1");

% Decision variables
% x[i, t, round] = 1 if person i is at table t in round round
array[1..v, 1..tables_per_round, 1..r] of var 0..1: x;

% C1: Each person at exactly one table per round
constraint forall(round in 1..r, person in 1..v)(
  sum(table in 1..tables_per_round)(x[person, table, round]) = 1
);

% C2: Each table has exactly k people
constraint forall(round in 1..r, table in 1..tables_per_round)(
  sum(person in 1..v)(x[person, table, round]) = k
);

% C3: Each pair of people shares a table at most once
% For each pair (i,j) with i < j, count how many times they are
% at the same table across all rounds. Must be ≤ 1.

% Linearization: for each pair (i,j), table t, round r:
%   if x[i,t,r]=1 and x[j,t,r]=1 then they share table t in round r

% Use auxiliary variables to count co-assignments.
array[1..v, 1..v] of var 0..r: pair_count;

% pair_count[i,j] = number of rounds where person i and j share a table
constraint forall(i in 1..v, j in i+1..v)(
  pair_count[i,j] = sum(round in 1..r)(
    sum(table in 1..tables_per_round)(
      x[i, table, round] * x[j, table, round]
    )
  )
);

% Each pair meets at most once (for resolvable design: exactly once)
constraint forall(i in 1..v, j in i+1..v)(
  pair_count[i,j] <= 1
);

% For strict resolvable design, each pair meet exatly once
% constraint forall(i in 1..v, j in i+1..v)(
%   pair_count[i,j] = 1
% );


% SYMMETRY BREAKING
% Fix person 1 to table 1 in round 1 (reduces search)
constraint forall(table in 2..tables_per_round)(
  x[1, table, 1] = 0
);

% Fix person 2 to table 1 in round 1 (they must meet person 1 early)
constraint forall(table in 2..tables_per_round)(
  x[2, table, 1] = 0
);

% SOLVE
solve satisfy;

% OUTPUT
output [
  "v=\(v), k=\(k), rounds=\(r), tables_per_round=\(tables_per_round)\n",
  "solution=\(x)\n"
];
