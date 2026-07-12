(import spork/test)

(import ../minizinc/solver :as solver)
(import ../minizinc/model :as model)
(import ../minizinc/instance :as instance)
(import ../minizinc/result :as result)

(test/start-suite 5)

(test/assert-no-error "set-param single key-value"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create "examples/nqueens.mzn"))
    (def inst (instance/create gecode m))
    (instance/set-param inst "n" 4)
    (assert (= (get (inst :params) "n") 4) "single param set")))

(test/assert-no-error "set-param multiple key-values"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create "examples/nqueens.mzn"))
    (def inst (instance/create gecode m))
    (instance/set-param inst "n" 8 "foo" "bar")
    (assert (= (get (inst :params) "n") 8) "first param")
    (assert (= (get (inst :params) "foo") "bar") "second param")))

(test/assert-no-error "solve nqueens n=4 (gecode)"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create "examples/nqueens.mzn"))
    (def inst (instance/create gecode m))
    (instance/set-param inst "n" 4)
    (def res (instance/solve inst))
    (assert (result/status-has-solution? (res :status)) "nqueens has solution")
    (assert (get (res :solution) "q") "solution has q")
    (def q ((res :solution) "q"))
    (assert (= (length q) 4) "q has 4 elements")))

(test/assert-no-error "solve nqueens all solutions"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create "examples/nqueens.mzn"))
    (def inst (instance/create gecode m))
    (instance/set-param inst "n" 4)
    (def res (instance/solve inst @{:all-solutions true}))
    (assert (= (res :status) :all-solutions) "status is ALL_SOLUTIONS")))

(test/assert-no-error "solve nqueens n=8 (chuffed)"
  (do
    (def chuffed (solver/lookup "chuffed"))
    (def m (model/create "examples/nqueens.mzn"))
    (def inst (instance/create chuffed m))
    (instance/set-param inst "n" 8)
    (def res (instance/solve inst))
    (assert (result/status-has-solution? (res :status)) "nqueens has solution")
    (def q ((res :solution) "q"))
    (assert q "solution has q")
    (assert (= (length q) 8) "q has 8 elements")))

(test/assert-no-error "solve sudoku"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create "examples/sudoku.mzn"))
    (def inst (instance/create gecode m))
    (instance/add-string inst "
      constraint x[1,1] = 5;
      constraint x[1,2] = 3;
      constraint x[1,4] = 6;
      constraint x[2,1] = 6;
      constraint x[2,3] = 1;
      constraint x[2,4] = 9;
      constraint x[2,5] = 5;
    ")
    (def res (instance/solve inst))
    (assert (result/status-has-solution? (res :status)) "sudoku has solution")
    (def x ((res :solution) "x"))
    (assert x "solution has x")
    (assert (= (get-in x [0 0]) 5) "x[1,1] = 5")
    (assert (= (get-in x [0 1]) 3) "x[1,2] = 3")))


(test/assert-no-error "branch incremental solving"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create))
    (model/add-string m "
      array[1..4] of var 1..10: x;
      var int: obj;
      constraint obj = sum(x);
      output [\"\\(obj)\"];
    ")
    (def inst (instance/create gecode m))
    (def res1 (instance/solve inst))
    (assert (result/status-has-solution? (res1 :status)) "branch: first solve")
    (def obj1 (get (res1 :solution) "obj"))
    (def child (instance/branch inst))
    (instance/add-string child (string "constraint obj > " obj1 ";"))
    (def res2 (instance/solve child))
    (assert (result/status-has-solution? (res2 :status)) "branch: second solve")
    (def obj2 (get (res2 :solution) "obj"))
    (assert (> obj2 obj1) "branch: second objective > first")))

(test/assert-no-error "inline model solve"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create))
    (model/add-string m "
      var 1..10: x;
      constraint x > 5;
      constraint x mod 2 = 0;
      solve satisfy;
    ")
    (def inst (instance/create gecode m))
    (def res (instance/solve inst))
    (assert (result/status-has-solution? (res :status)) "inline model solved")
    (def x ((res :solution) "x"))
    (assert (> x 5) "x > 5")
    (assert (= (mod x 2) 0) "x is even")))

(test/assert-no-error "matrix output 2D array"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create))
    (model/add-string m "
      array[1..3, 1..3] of var 1..9: grid;
      constraint grid[1,1] = 1;
      constraint grid[2,2] = 5;
      constraint grid[3,3] = 9;
      solve satisfy;
    ")
    (def inst (instance/create gecode m))
    (def res (instance/solve inst))
    (assert (result/status-has-solution? (res :status)) "matrix solve")
    (def g ((res :solution) "grid"))
    (assert g "grid exists")
    (assert (= (type g) :array) "grid is array")
    (assert (= (length g) 3) "grid has 3 rows")
    (assert (= (type (first g)) :array) "row is array")
    (assert (= (length (first g)) 3) "row has 3 cols")
    (assert (= (get-in g [0 0]) 1) "grid[1,1] = 1")
    (assert (= (get-in g [1 1]) 5) "grid[2,2] = 5")
    (assert (= (get-in g [2 2]) 9) "grid[3,3] = 9")))

(test/assert-no-error "error on bad model"
  (do
    (def gecode (solver/lookup "gecode"))
    (def m (model/create))
    (model/add-string m "Not a valid minizinc model")
    (def inst (instance/create gecode m))
    (test/assert-error "bad model errors" (instance/solve inst))))

(test/end-suite)
