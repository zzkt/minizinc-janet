# Group meeting arragements with flexible group sizes
#
# Assigns people to groups across multiple rounds with possibly
# varying group sizes. Minimizes repeated pairings.
#
# Usage:
#   janet group-meeting-flex.janet --people 12
#   janet group-meeting-flex.janet --demo kirkman
#   janet group-meeting-flex.janet --rounds "3,3,3|4,4,4|6,6"

(import minizinc/solver :as solver)
(import minizinc/model :as model)
(import minizinc/instance :as instance)

# solver to use with any config options
(var *solver* "cp-sat") # try 'chuffed' or 'gecode' or 'cp-sat'
(var *solver-config* @{:processes 16
                       :free-search true})

# Demos: @[:people N :rounds @[@[...] ...] :desc "..."]
(def presets @{
  :nines @{:people 9 :rounds @[@[3 3 3] @[4 5] @[9]]
          :desc "9 people: 3→mixed→all"}

  :classroom @{:people 12 :rounds @[@[3 3 3 3] @[4 4 4] @[6 6]]
               :desc "12 people: 3→4→6"}

  # "Fifteen young ladies in a school walk out three abreast for seven days in succession:
  #  it is required to arrange them daily so that no two shall walk twice abreast."
  # -- Thomas Kirkman, 1850
  :kirkman @{:people 15 :rounds (seq [_ :range [0 7]] @[3 3 3 3 3])
             :desc "15 people: 3 abreast for 7 days (Kirkman's schoolgirls)"}

  :dagstuhl @{:people 9 :rounds @[@[3 3 3] @[3 3 3] @[3 3 3] @[3 3 3]]
              :desc "9 people: Kirkman triple system, T(9,3)=4"}})


(def default-names
  @["Aiko" "Boris" "Chen" "Deepa" "Emeka" "Fatima" "Gonzalo"
    "Hiroshi" "Ingrid" "Javier" "Kim" "Leila" "Mikhail"
    "Nadia" "Oscar" "Priya" "Quinn" "Rashid" "Sakiko"
    "Tariq" "Uma" "Viktor" "Winona" "Xavier" "Yuki" "Zara"])


(defn parse-rounds
```Parse group/rounds specifications
Group sizes are comma separated, rounds are separated by pipes. Use 'x' to repeat.

  "3,3,3|4,5"  → @[@[3 3 3] @[4 5]]
  "3x3|4,5"    → @[@[3 3 3] @[4 5]]
  "3x5x7"      → @[@[3 3 3 3 3] @[3 3 3 3 3] ...] (i.e. :kirkman)
  "2..."       → as many groups of 2 as required```
  [s]
  (def rounds @[])
  (each rs (string/split "|" s)
    (var sizes @[])
    (each tok (string/split "," rs)
      (def t (string/trim tok))
      (cond
        # "..." → mark for later expansion
        (= t "...") (array/push sizes :fill)
        # repeat current round n times
        (string/has-prefix? "x" t)
          (do (def n (scan-number (string/slice t 1)))
              (for _ 0 n (array/push rounds (array/slice sizes)))
              (set sizes @[]))
        # "2..." → groups of size 2, fill to seat everyone
        (string/has-suffix? "..." t)
          (do (def val (scan-number (string/slice t 0 -4)))
              (array/push sizes val))
        # "3" or "3x4" or "3x4x7" → parse number, repeat, or round-repeat
        (do (def parts (string/split "x" t))
            (cond
              (= (length parts) 3)
                (do (def val (scan-number (get parts 0)))
                    (def cnt (scan-number (get parts 1)))
                    (def rep (scan-number (get parts 2)))
                    (def round @[])
                    (for _ 0 cnt (array/push round val))
                    (for _ 0 rep (array/push rounds (array/slice round))))
              (= (length parts) 2)
                (do (def val (scan-number (get parts 0)))
                    (def cnt (scan-number (get parts 1)))
                    (for _ 0 cnt (array/push sizes val)))
              (array/push sizes (scan-number t))))))
    (when (> (length sizes) 0)
      (array/push rounds sizes)))
  rounds)


# Expand "..." markers: N... → ceil(people/N) copies of N
(defn expand-rounds [rounds people]
  (def result @[])
  (each round rounds
    (if (not (find (fn [x] (= x :fill)) round))
      (array/push result round)
      (do
        (def val (first round))
        (def n-groups (math/ceil (/ people val)))
        (def expanded (array/new n-groups val))
        (array/push result expanded))))
  result)


# Parse CLI args → config dict
(defn parse-args [args]
  (var people nil)
  (var rounds nil)
  (var preset nil)
  (var names nil)
  (var compact false)
  (var svg nil)
  (var time-limit 120000)
  (var list-demos false)
  (var i 1)
  (while (< i (length args))
    (def arg (args i))
    (cond
      (= arg "--people")  (do (++ i) (set people (scan-number (args i))) (++ i))
      (= arg "--rounds")  (do (++ i) (set rounds (parse-rounds (args i))) (++ i))
      (= arg "--demo")
        (do (++ i)
            (if (and (< i (length args)) (not (string/has-prefix? "--" (args i))))
              (do (set preset (keyword (args i))) (++ i))
              (set list-demos true)))
      (= arg "--compact") (do (set compact true) (++ i))
      (= arg "--svg")     (do (++ i) (set svg (args i)) (++ i))
      (= arg "--time-limit") (do (++ i) (set time-limit (scan-number (args i))) (++ i))
      (= arg "--names")
        (do (set names @[]) (++ i)
            (while (and (< i (length args)) (not (string/has-prefix? "--" (args i))))
              (array/push names (args i)) (++ i)))
      (++ i)))
  (when preset
    (def p (get presets preset))
    (when p (set people (get p :people)) (set rounds (get p :rounds))))
  (def n (or names (array/slice default-names 0 (or people 9))))
  @{:people (or people (length n)) :names n
    :rounds (or rounds @[@[3 3 3] @[4 5] @[9]])
    :compact compact :svg svg :time-limit time-limit :list-demos list-demos})


(defn show-config [people rounds]
  (printf "People: %d\nRounds: %d" people (length rounds))
  (eachp [i round] rounds
    (def total (reduce + 0 round))
    (def sizes (string/join (map string round) "+"))
    (cond
      (< total people) (printf "  R%d: %s = %d [%d extra]" (+ i 1) sizes total (- people total))
      (> total people) (printf "  R%d: %s = %d [%d extra capacity]" (+ i 1) sizes total (- total people))
      (printf "  R%d: %s = %d" (+ i 1) sizes total))))


(defn find-assignments
  " Find group assignments for a person: @[@[group round] ...]"
  [x person rounds]
  (def result @[])
  (eachp [r-idx round] rounds
    (var group-idx 0)
    (each _ round
      (when (= 1 (get (get (get x person) group-idx) r-idx))
        (array/push result @[group-idx r-idx]))
      (++ group-idx)))
  result)


(defn show-seating
  "Print seating arrangement."
  [solution people names rounds]
  (def x (get solution "x"))
  (def extra (get solution "extra"))
  (printf "\nMeeting Arrangement:")
  (eachp [r-idx round] rounds
    (def actual-sizes @[])
    (var tidx 0)
    (each _ round
      (var count 0)
      (for p 0 people (when (= 1 (get (get (get x p) tidx) r-idx)) (++ count)))
      (array/push actual-sizes count) (++ tidx))
    (def extras (- people (reduce + 0 actual-sizes)))
    (printf "\nRound %d (groups: %s)%s: " (+ r-idx 1)
      (string/join (map string actual-sizes) ", ")
      (if (> extras 0) (string/format " [%d extra]" extras) ""))
    (var tidx 0)
    (def extras-list @[])
    (each _ round
      (def members @[])
      (for p 0 people
        (when (= 1 (get (get (get x p) tidx) r-idx))
          (array/push members (get names p))))
      (cond
        (= 0 (length members)) nil
        (= 1 (length members)) (array/push extras-list (first members))
        (printf "  Group %d: %s" (+ tidx 1) (string/join members ", ")))
      (++ tidx))
    (when (> (length extras-list) 0)
      (printf "  Extra:   %s" (string/join extras-list ", ")))
    (when (and extra (> extras 0))
      (def extras-list @[])
      (for p 0 people
        (when (= 1 (get (get extra p) r-idx))
          (array/push extras-list (get names p))))
      (when (> (length extras-list) 0)
        (printf "  Extra:   %s" (string/join extras-list ", "))))))


(defn show-compact
  "Print compact ouput."
  [solution people names rounds]
  (def x (get solution "x"))
  (def extra (get solution "extra"))
  (printf "\nCompact (name; group per round):")
  (for p 0 people
    (def assignments @[])
    (eachp [r-idx round] rounds
      (var tidx 0) (var seated false)
      (each _ round
        (when (= 1 (get (get (get x p) tidx) r-idx))
          (array/push assignments (+ tidx 1)) (set seated true))
        (++ tidx))
      (unless seated
        (if extra (array/push assignments "extra") (array/push assignments "?"))))
    (printf "  %s; %s" (get names p) (string/join (map string assignments) ", "))))


(defn count-pairs [solution people names rounds]
  (def pair-count @{})
  (def x (get solution "x"))
  (eachp [r-idx round] rounds
    (var tidx 0)
    (each _ round
      (def members @[])
      (for p 0 people
        (when (= 1 (get (get (get x p) tidx) r-idx))
          (array/push members (get names p))))
      (for i 0 (length members)
        (for j (+ i 1) (length members)
          (def key (if (< (members i) (members j))
                     (string (members i) ":" (members j))
                     (string (members j) ":" (members i))))
          (put pair-count key (+ 1 (or (get pair-count key) 0)))))
      (++ tidx)))
  pair-count)


(defn show-pairs
  "Print statistics for pairs."
  [pair-count people names]
  (def total-pairs (/ (* people (- people 1)) 2))
  (def pairs-met (length pair-count))
  (def not-met (- total-pairs pairs-met))
  (def once (length (filter (fn [kv] (= (kv 1) 1)) (pairs pair-count))))
  (def multi (length (filter (fn [kv] (> (kv 1) 1)) (pairs pair-count))))
  (printf "\nPair Coverage:")
  (printf "  Total pairs: %d" total-pairs)
  (printf "  Met: %d (%d%%)" pairs-met (math/floor (* 100 (/ pairs-met total-pairs))))
  (printf "  Exactly once: %d" once)
  (printf "  More than once: %d" multi)
  (when (> multi 0)
    (printf "\nRepeated:")
    (eachp [k v] pair-count
      (when (> v 1)
        (def parts (string/split ":" k))
        (printf "  %s ↔ %s: %d" (parts 0) (parts 1) v))))
  (when (> not-met 0)
    (printf "\nDid not meet:")
    (for i 0 (length names)
      (for j (+ i 1) (length names)
        (def key (if (< (names i) (names j))
                   (string (names i) ":" (names j))
                   (string (names j) ":" (names i))))
        (unless (get pair-count key)
          (printf "  %s ↔ %s" (names i) (names j)))))))


# Generate SVG visualization
(defn generate-svg [solution people names rounds filename]
  (def buf @"")
  (def x (get solution "x"))
  (def extra (get solution "extra"))
  (def num-rounds (length rounds))
  (def svg-width 1200)
  (def round-height 300)
  (def svg-height (+ 100 (* num-rounds round-height)))
  (def colors @["#e8e0f0" "#e0ecf0" "#e0f0ec" "#e0f0e0" "#f0f0e0"
                "#f0ece0" "#e8e0f0" "#e0ecf0" "#e0f0ec" "#e0f0e0"])

  (buffer/push-string buf (string/format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" style=\"width:100%%;height:auto\">\n" svg-width svg-height))
  (buffer/push-string buf "<style>\n")
  (buffer/push-string buf "  text { font-family: Arial, sans-serif; }\n")
  (buffer/push-string buf "  .title { font-size: 24px; font-weight: bold; }\n")
  (buffer/push-string buf "  .round-label { font-size: 18px; font-weight: bold; }\n")
  (buffer/push-string buf "  .group-label { font-size: 16px; fill: #333; text-anchor: middle; dominant-baseline: central; }\n")
  (buffer/push-string buf "  .person { font-size: 13px; fill: #000; text-anchor: middle; }\n")
  (buffer/push-string buf "</style>\n")

  (buffer/push-string buf (string/format "<text x=\"600\" y=\"30\" text-anchor=\"middle\" class=\"title\">Group Meeting (%d people)</text>\n" people))

  (var round-y 60)
  (eachp [r-idx round] rounds
    (def round-num (+ r-idx 1))
    (def num-groups (length round))

    # Calculate actual group sizes
    (def actual-sizes @[])
    (var tidx 0)
    (each _ round
      (var count 0)
      (for p 0 people (when (= 1 (get (get (get x p) tidx) r-idx)) (++ count)))
      (array/push actual-sizes count) (++ tidx))
    (def extras (- people (reduce + 0 actual-sizes)))

    # Round label
    (buffer/push-string buf (string/format "<text x=\"20\" y=\"%d\" class=\"round-label\">Round %d: (groups: %s)%s</text>\n"
      round-y round-num (string/join (map string actual-sizes) ", ")
      (if (> extras 0) (string/format " [%d extra]" extras) "")))

    # Group positions
    (def group-spacing (math/floor (/ (- svg-width 80) num-groups)))
    (var group-x (+ 40 (math/floor (/ group-spacing 2))))

    # Draw groups
    (var tidx 0)
    (each _ round
      (def color (get colors (% tidx (length colors))))
      (def radius (+ 25 (* (get round tidx) 7)))

      # Group circle
      (buffer/push-string buf (string/format "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"%s\" stroke=\"#666\" stroke-width=\"2\"/>\n"
        group-x (+ round-y 160) radius color))

      # Group number
      (buffer/push-string buf (string/format "<text x=\"%d\" y=\"%d\" class=\"group-label\" font-size=\"16\" font-weight=\"bold\">G%d</text>\n"
        group-x (+ round-y 160) (+ tidx 1)))

      # People arranged in a circle
      (def people-at-group @[])
      (for p 0 people
        (when (= 1 (get (get (get x p) tidx) r-idx))
          (array/push people-at-group (get names p))))
      (def n (length people-at-group))
      (when (> n 0)
        (def angle-step (/ (* 2 math/pi) n))
        (var person-idx 0)
        (each name people-at-group
          (def angle (- (* person-idx angle-step) (/ math/pi 2)))
          (def person-radius (+ radius 25))
          (def px (+ group-x (* person-radius (math/cos angle))))
          (def py (+ (+ round-y 160) (* person-radius (math/sin angle))))
          (buffer/push-string buf (string/format "<text x=\"%.1f\" y=\"%.1f\" class=\"person\">%s</text>\n"
            px (+ py 4) name))
          (++ person-idx)))

      (set group-x (+ group-x group-spacing))
      (++ tidx))

    # Draw extras
    (when (and extra (> extras 0))
      (def extra-people @[])
      (for p 0 people
        (when (= 1 (get (get extra p) r-idx))
          (array/push extra-people (get names p))))
      (when (> (length extra-people) 0)
        (def extra-x (- svg-width 100))
        (def extra-y (+ round-y 160))
        (buffer/push-string buf (string/format "<rect x=\"%d\" y=\"%d\" width=\"80\" height=\"30\" rx=\"5\" fill=\"#FFECB3\" stroke=\"#666\" stroke-width=\"1\"/>\n"
          extra-x (- extra-y 15)))
        (buffer/push-string buf (string/format "<text x=\"%d\" y=\"%d\" class=\"group-label\" font-size=\"12\">Extra</text>\n"
          (+ extra-x 40) extra-y))
        (var ey (+ extra-y 30))
        (each name extra-people
          (buffer/push-string buf (string/format "<text x=\"%d\" y=\"%d\" class=\"person\" text-anchor=\"middle\">%s</text>\n"
            (+ extra-x 40) ey name))
          (+= ey 15))))
    (set round-y (+ round-y round-height)))
  (buffer/push-string buf "</svg>\n")
  (spit filename (string buf))
  (printf "\nSVG written to: %s" filename))


(defn main [& args]
  # Handle --help before further parsing
  (when (find (fn [a] (= a "--help")) args)
    (printf "Usage: group-meeting-flex [--people N] [--rounds SPEC] [--demo NAME]")
    (printf "       [--names A B C] [--compact] [--svg FILE] [--time-limit MS]\n")
    (eachp [k v] presets (printf "  %s: %s" k (get v :desc)))
    (break))
  (def config (parse-args args))
  (when (config :list-demos)
    (printf "Available demos:")
    (eachp [k v] presets (printf "  --demo %-12s %s" k (get v :desc)))
    (break))
  (def people (config :people))
  (def names (config :names))
  (def rounds (config :rounds))
  (print "Group configuration:\n")
  (show-config people rounds)

  # Select model based on whether extras are needed
  (def needs-extras (not= nil (find (fn [r] (not= (reduce + 0 r) people)) rounds)))
  (def model-file (if needs-extras
                    "group-meeting-extras.mzn"
                    "group-meeting-flex.mzn"))
  (printf "Solver: %s" *solver*)
  (printf "Model: %s" (if needs-extras "extras" "exact"))

  # Solve
  (def sol (solver/lookup *solver*))
  (def inst (instance/create sol (model/create model-file)))
  (def max-groups (max ;(map length rounds)))
  (def sizes @[])
  (each round rounds
    (def padded (array/slice round))
    (while (< (length padded) max-groups) (array/push padded 0))
    (array/push sizes padded))
  (instance/set-param inst
                      "v" people
                      "R" (length rounds)
                      "max_tables" max-groups
                      "sizes" sizes)

  (printf "\nSolving...")
  (def res (instance/solve inst
                           (merge @{:time-limit (config :time-limit)}
                                  *solver-config*)))
  (printf "Status: %v" (res :status))
  (def sol (res :solution))

  (when (or (= (res :status) :optimal-solution)
            (= (res :status) :satisfied))
    (def extras-val (get sol "total_extras"))
    (when extras-val (printf "Extras: %v" extras-val))
    (printf "Repeats: %v" (get sol "repeats"))
    (show-seating sol people names rounds)
    (show-compact sol people names rounds)
    (show-pairs (count-pairs sol people names rounds) people names)
    (when (config :svg)
      (generate-svg sol people names rounds (config :svg))))
  (when (= (res :status) :unknown)
    (printf "No solution found (time limit exceeded or unsolvable)")))
