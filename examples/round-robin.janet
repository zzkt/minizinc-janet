# -*- mode: janet; -*-
# Round-Robin Tournament Schedule (Polygon/Circle Method)
#
# Generates optimal round-robin tournament schedules where each pair
# meets exactly once. Uses the polygon (circle) method algorithm.
#
# Algorithm: O(v²) time complexity
#   - For even n people: n-1 rounds, n/2 pairs per round
#   - For odd n people: n rounds, (n-1)/2 pairs per round, one sits out
#   - Player 0 is fixed, others rotate around a polygon
#   - Opposite vertices are paired each round
#
# This generally outperforms onstraint solving, especially for large groups.
# An example minzinc model is included for comparrison

# Usage:
#   janet round-robin.janet
#   janet round-robin.janet --people 20
#   janet round-robin.janet --names Aiko Boris Chen ...


# Default names
(def default-names
  @["Aiko" "Boris" "Chen" "Deepa" "Emeka" "Fatima" "Gonzalo"
    "Hiroshi" "Ingrid" "Javier" "Kim" "Leila" "Mikhail"
    "Nadia" "Oscar" "Priya" "Quinn" "Rashid" "Sakiko"
    "Tariq" "Uma" "Viktor" "Winona" "Xavier" "Yuki" "Zara"])


# Parse CLI args
(defn parse-args
  [args]
  (var names nil)
  (var people-count nil)
  (var i 1)
  (while (< i (length args))
    (def arg (args i))
    (cond
      (= arg "--names")
        (do
          (set names @[])
          (++ i)
          (while (and (< i (length args))
                      (not (string/has-prefix? "--" (args i))))
            (array/push names (args i))
            (++ i)))
      (= arg "--people")
        (do (++ i) (set people-count (scan-number (args i))) (++ i))
      (do (++ i))))
  # Generate names if --people specified
  (def people (if people-count
                (do
                  (def name-list (array/slice default-names))
                  (def shuffled (map (fn [x] [x (math/random)]) name-list))
                  (sort shuffled (fn [a b] (< (a 1) (b 1))))
                  (seq [i :in (range people-count)] ((shuffled i) 0)))
                (or names (array/slice default-names))))
  @{:names people})


# Circle method for round-robin tournament allocation
(defn generate-round-robin
  "Generate round-robin schedule using circle method. O(v²) time."
  [people]
  (def n (length people))
  (def rounds @[])
  (def is-odd (= 1 (% n 2)))
  (def v (if is-odd (+ n 1) n))
  (def R (- v 1))
  # Create list with placeholder for odd numbered group
  (def players (array/slice people))
  (when is-odd
    (array/push players "(sit out)"))
  # modulo that returns 0..m-1
  (defn mod [a m] (% (+ m (% a m)) m))
  # Circle method: player 0 fixed, players 1..v-1 rotate
  # In round r, position i (0..v-1) has a player
  # Position 0 always has player 0
  # Position i (1..v-1) has player ((i-1-r) mod (v-1)) + 1
  (for r 0 R
    (def round-pairs @[])
    # Pair position 0 with position v/2
    (def opp-pos (/ v 2))
    (def opp-player (mod (- (- opp-pos 1) r) (- v 1)))
    (def p0 (get players 0))
    (def p-opp (get players (+ 1 opp-player)))
    (when (not= p-opp "(sit out)")
      (array/push round-pairs [p0 p-opp]))
    # Pair remaining positions: i with v-i for i=1..v/2-1
    (for i 1 (/ v 2)
      # paired position
      (def j (- v i))
      # Which player is at position i?
      (def pi (mod (- (- i 1) r) (- v 1)))
      (def player-i (+ 1 pi))
      # Which player is at position j?
      (def pj (mod (- (- j 1) r) (- v 1)))
      (def player-j (+ 1 pj))
      (def p1 (get players player-i))
      (def p2 (get players player-j))
      # skip the pair if a person sits out
      (cond
        (= p1 "(sit out)")  nil
        (= p2 "(sit out)")  nil
        true
          (array/push round-pairs [p1 p2])))
    (array/push rounds round-pairs))
  rounds)


(defn print-schedule
  "Print the schedule."
  [people rounds]
  (def n (length people))
  (def total-unique-pairs (/ (* n (- n 1)) 2))
  (def is-odd (= 1 (% n 2)))
  (printf "Total unique pairs: %d" total-unique-pairs)
  (printf "Rounds needed: %d\n" (length rounds))
  (var r 0)
  (each round rounds
    (++ r)
    # does anyone sit out this round?
    (def seated @{})
    (each pair round
      (put seated (pair 0) true)
      (put seated (pair 1) true))
    (def extras (filter (fn [p] (not (get seated p))) people))
    (printf "Round %d:" r)
    (each pair round
      (printf "  %s & %s" (pair 0) (pair 1)))
    (when (> (length extras) 0)
      (printf "  (sits out: %s)" (string/join extras ", ")))
    (printf "")))


(defn main
  [& args]
  (def config (parse-args args))
  (def people (config :names))
  (def n (length people))
  (print "Round-Robin Polygon")
  (print "===================\n")
  (printf "People (%d): %s\n" n (string/join people ", "))
  # Generate schedule
  (def rounds (generate-round-robin people))
  # Print schedule
  (print-schedule people rounds))
