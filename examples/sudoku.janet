# -*- mode: janet; -*-
# Sudoku Solver
#
# Usage:
#   janet sudoku.janet --puzzle easy
#   janet sudoku.janet --set "0,0,0,2,6,0,7,0,1,..."

(import minizinc/solver :as solver)
(import minizinc/model :as model)
(import minizinc/instance :as instance)

# Puzzles: 0 = empty cell, row-major
(def puzzles @{
  :easy @[0 0 0 2 6 0 7 0 1
          6 8 0 0 7 0 0 9 0
          1 9 0 0 0 4 5 0 0
          8 2 0 1 0 0 0 4 0
          0 0 4 6 0 2 9 0 0
          0 5 0 0 0 3 0 2 8
          0 0 9 3 0 0 0 7 4
          0 4 0 0 5 0 0 3 6
          7 0 3 0 1 8 0 0 0]
  :medium @[0 0 0 0 0 0 0 0 0
            0 0 0 0 0 3 0 8 5
            0 0 1 0 2 0 0 0 0
            0 0 0 5 0 7 0 0 0
            0 0 4 0 0 0 1 0 0
            0 9 0 0 0 0 0 0 0
            5 0 0 0 0 0 0 7 3
            0 0 2 0 1 0 0 0 0
            0 0 0 0 4 0 0 0 9]
  :hard @[8 0 0 0 0 0 0 0 0
          0 0 3 6 0 0 0 0 0
          0 7 0 0 9 0 2 0 0
          0 5 0 0 0 7 0 0 0
          0 0 0 0 4 5 7 0 0
          0 0 0 1 0 0 0 3 0
          0 0 1 0 0 0 0 6 8
          0 0 8 5 0 0 0 1 0
          0 9 0 0 0 0 4 0 0]})

# Print sudoku grid
(defn print-grid [grid]
  (def buf @"")
  (def top    "  ╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗")
  (def mid-h  "  ╟───┼───┼───╫───┼───┼───╫───┼───┼───╢")
  (def mid-b  "  ╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣")
  (def bottom "  ╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝")
  (buffer/push-string buf top "\n")
  (var r 0)
  (each row grid
    (++ r)
    (buffer/push-string buf "  ║")
    (var c 0)
    (each val row
      (++ c)
      (if (= val 0)
        (buffer/push-string buf "   ")
        (buffer/push-string buf (string " " val " ")))
      (if (= c 9)
        (buffer/push-string buf "║")
        (if (= 0 (% c 3))
          (buffer/push-string buf "║")
          (buffer/push-string buf "│"))))
    (buffer/push-string buf "\n")
    (when (< r 9)
      (buffer/push-string buf (if (= 0 (% r 3)) mid-b mid-h) "\n")))
  (buffer/push-string buf bottom "\n")
  (print (string buf)))

# Main
(defn main [& args]
  (var puzzle-key :easy)
  (var custom nil)
  (var i 1)
  (while (< i (length args))
    (cond
      (= (args i) "--puzzle")
        (do (++ i) (set puzzle-key (keyword (args i))) (++ i))
      (= (args i) "--set")
        (do (++ i) (set custom (args i)) (++ i))
      (= (args i) "--help")
        (do (print "Usage: sudoku [--puzzle NAME] [--set CELLS]\n")
            (print "Puzzles: easy, medium, hard, expert")
            (break))
      (++ i)))
  # Get puzzle
  (def puzzle (if custom
                custom
                (string/join (map string (get puzzles puzzle-key)) ",")))
  (def nums @[])
  (each s (string/split "," puzzle)
    (each part (string/split " " (string/trim s))
      (when (> (length part) 0)
        (array/push nums (scan-number part)))))
  (when (not= (length nums) 81)
    (printf "Error: need 81 cells, got %d" (length nums))
    (break))
  # Display puzzle
  (def initial @[])
  (for r 0 9
    (array/push initial (array/slice nums (* r 9) (+ (* r 9) 9))))
  (print "Puzzle:")
  (print-grid initial)
  # Solve
  (def inst (instance/create
             (solver/lookup "gecode")
             (model/create "sudoku.mzn")))
  (for r 0 9
    (for c 0 9
      (def val (get nums (+ (* r 9) c)))
      (when (> val 0)
        (instance/add-string
         inst
         (string/format "constraint x[%d,%d] = %d;" (+ r 1) (+ c 1) val)))))
  (def result (instance/solve inst))
  (printf "\nStatus: %v\n" (result :status))
  (when (= (result :status) :satisfied)
    (print "Solution:")
    (print-grid (get (result :solution) "x"))))
