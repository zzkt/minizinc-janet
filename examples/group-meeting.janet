# Table Seating Problem as Resolvable Block Design
#
# Solves: Given v people arranged into groups of size k, find rounds
#         where every pair of people shares a group exactly once.
#
# The problem is expressed as a resolvable (v,k,1)2-design
#  - In the strict (exact) version, every pair must meet exactly once
#  - Upper Version, every pair meets at least once
#  - Lower Version, as many different pairs as possible meet
#
# Variations on upper/lower resolutions are explored in other examples.

# Usage:
#   janet group-meeting.janet              # default: 9 people, groups of 3
#   janet group-meeting.janet 15 3         # Kirkman's Schoolgirl Problem
#   janet group-meeting.janet 8 4          # 8 people, groups of 4
#   janet group-meeting.janet --all        # find all solutions

(import minizinc/solver :as solver)
(import minizinc/model :as model)
(import minizinc/instance :as instance)

# solver to use
(var *solver* "cp-sat") # try 'chuffed' or 'gecode' or 'cp-sat'

# Parse arguments
(defn parse-args [args]
  (var v nil)
  (var k nil)
  (var all-solutions false)
  (var i 1)
  (while (< i (length args))
    (cond
      (= (args i) "--all")
        (do (set all-solutions true) (++ i))
      (= (args i) "--help")
        (do
          (print "Usage: group-meeting [v] [k] [--all]")
          (print "  v: number of people (default: 9)")
          (print "  k: group capacity (default: 3)")
          (print "  --all: find all solutions")
          (break))
      (do
        (if (nil? v)
          (set v (scan-number (args i)))
          (set k (scan-number (args i))))
        (++ i))))
  {:v (or v 9) :k (or k 3) :all-solutions all-solutions})


(defn validate-params
  "Validate design parameters"
  [v k]
  (printf "Checking resolvable (%d, %d, 1)-design conditions: " v k)
  (def cond1 (= 0 (% v k)))
  (def cond2 (= 0 (% (- v 1) (- k 1))))
  (printf "  1. v ≡ 0 (mod k):        %d ≡ %d (mod %d) = %s "
    v (% v k) k (if cond1 "✓" "✗"))
  (printf "  2. v-1 ≡ 0 (mod k-1):    %d ≡ %d (mod %d) = %s "
    (- v 1) (% (- v 1) (- k 1)) (- k 1) (if cond2 "✓" "✗"))
  (when (and cond1 cond2)
    (def rounds (/ (- v 1) (- k 1)))
    (def groups (/ v k))
    (printf "  → Rounds needed: %d" rounds)
    (printf "  → Groups per round: %d" groups)
    (printf "  → Total pairs covered: %d / %d"
      (* rounds groups (/ (* k (- k 1)) 2))
      (/ (* v (- v 1)) 2)))
  (and cond1 cond2))


(defn print-seating
  "Format solution as seating chart"
  [solution v k rounds groups-per-round]
  (def x (get solution "x"))
  (print "\nMeeting Schedule:")
  (for round 1 (+ rounds 1)
    (printf "Round %d:" round)
    (for group 1 (+ groups-per-round 1)
      (def people @[])
      (for person 1 (+ v 1)
        (when (= 1 (get (get (get x (- person 1)) (- group 1)) (- round 1)))
          (array/push people person)))
      (printf "  Group %d:  %s" group
              (string/join (map (fn [x] (string x)) people) ", ")))
    (printf "")))


(defn verify-pairs
  "Verify all pairs meet exactly once"
  [solution v k rounds groups-per-round]
  (def x (get solution "x"))
  (def pair-count @{})
  (for round 1 (+ rounds 1)
    (for group 1 (+ groups-per-round 1)
      (def people @[])
      (for person 1 (+ v 1)
        (when (= 1 (get (get (get x (- person 1)) (- group 1)) (- round 1)))
          (array/push people person)))
      # Count all pairs in this group
      (for i 0 (length people)
        (for j (+ i 1) (length people)
          (def a (min (people i) (people j)))
          (def b (max (people i) (people j)))
          (def key (string a ":" b))
          (put pair-count key (+ 1 (or (get pair-count key) 0)))))))
  # Do all pairs appear exactly once?
  (var all-ok true)
  (for i 1 (+ v 1)
    (for j (+ i 1) (+ v 1)
      (def key (string i ":" j))
      (def count (get pair-count key 0))
      (when (not= count 1)
        (printf "  PAIR %d-%d: %d times (expected 1)" i j count)
        (set all-ok false))))
  all-ok)


(defn main [& args]
  (def config (parse-args args))
  (def v (config :v))
  (def k (config :k))

  (print "\nGroup Meeting Problem\n")
  (printf "People: %d, Group size: %d\n" v k)

  # Validate
  (unless (validate-params v k)
    (printf "\nCannot form a resolvable (%d, %d, 1)-design." v k)
    (printf "Requirements: v ≡ 0 (mod k) and v-1 ≡ 0 (mod k-1)")
    (break))

  # Load model
  (def s (solver/lookup *solver*))
  (def m (model/create "group-meeting.mzn"))
  (def inst (instance/create s m))

  # Set parameters
  (instance/set-param inst "v" v "k" k)

  # Solve
  (printf "\nSolving...")
  (def res (instance/solve
            inst @{:all-solutions (config :all-solutions)}))

  # Print output
  (if (config :all-solutions)
    (do
      (printf "Found %d solutions" (length (get res :solutions [])))
      (each sol (get res :solutions [])
        (print-seating sol v k (/ (- v 1) (- k 1)) (/ v k))))
    (do
      (printf "Status: %v" (res :status))
      (when (or (= (res :status) :satisfied)
                (= (res :status) :optimal-solution))
        (print-seating (res :solution) v k (/ (- v 1) (- k 1)) (/ v k))
        (printf "Verifying all pairs meet exactly once...")
        (if (verify-pairs (res :solution) v k (/ (- v 1) (- k 1)) (/ v k))
          (printf "✓ All pairs verified!")
          (printf "✗ Some pairs missed or repeated"))))))
