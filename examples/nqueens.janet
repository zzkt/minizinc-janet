# -*- mode: janet; -*-
# N-Queens Solver
#
# Usage:
#   janet nqueens.janet 8         # solve 8-queens
#   janet nqueens.janet 8 --all   # find all solutions

(import minizinc/solver :as solver)
(import minizinc/model :as model)
(import minizinc/instance :as instance)
(import minizinc/result :as result)


(defn print-board
  "Print the board by placing Queens."
  [q n]
  (def board @"")
  (var row 0)
  (each col q
    (++ row)
    (buffer/push-string board "  ")
    (for c 1 (+ n 1)
      (when (> c 1) (buffer/push-string board " "))
      (if (= c col)
        (buffer/push-string board "♛")
        (buffer/push-string board (if (= 0 (% (+ row c) 2)) "⬛" "⬜"))))
    (buffer/push-string board "\n"))
  (prin (string board)))


(defn format-queens
  "Format queens array as string"
  [q]
  (def buf @"[")
  (var first true)
  (each v q
    (if first
      (do (buffer/push-string buf (string v)) (set first false))
      (buffer/push-string buf (string ", " v))))
  (buffer/push-string buf "]")
  (string buf))


(defn main [& args]
  # Parse arguments
  (var board-size 8)
  (var all-solutions false)
  (var i 1)
  (while (< i (length args))
    (cond
      (= (args i) "--all") (do (set all-solutions true) (++ i))
      (= (args i) "--help")
        (do (print "Usage: nqueens [SIZE] [--all]")
            (print "  SIZE: board size (default: 8)")
            (print "  --all: find all solutions")
            (break))
      (do (set board-size (scan-number (args i))) (++ i))))

  # Solve
  (def gecode (solver/lookup "gecode"))
  (def m (model/create "nqueens.mzn"))
  (def inst (instance/create gecode m))
  (instance/set-param inst "n" board-size)

  (def opts (if all-solutions @{:all-solutions true} @{}))
  (def res (instance/solve inst opts))

  (printf "n-Queens (n=%d)" board-size)
  (printf "Status: %v" (res :status))

  (when (result/status-has-solution? (res :status))
    (if all-solutions
      # Print all solutions
      (do
        (def solutions (get res :solutions []))
        (printf "Found %d solutions\n" (length solutions))
        (each sol solutions
          (def q (get sol "q"))
          (printf "Queens: %s\n" (format-queens q))
          (print-board q board-size)
          (print)))
      # Print single solution
      (do
        (def q (get (res :solution) "q"))
        (printf "Queens: %s\n" (format-queens q))
        (print-board q board-size)))))
