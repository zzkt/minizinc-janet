# -*- mode: janet; -*-
# minizinc: results and output

(import spork/json)

(defn parse-status
  "Parse a MiniZinc status string to a keyword."
  [status-str]
  (case status-str
    "ERROR" :error
    "UNKNOWN" :unknown
    "UNBOUNDED" :unbounded
    "UNSATISFIABLE" :unsatisfiable
    "SATISFIED" :satisfied
    "ALL_SOLUTIONS" :all-solutions
    "OPTIMAL_SOLUTION" :optimal-solution
    :error))

(defn status-has-solution?
  "Check if a status indicates a solution was found."
  [status]
  (not (nil? (find (fn [s] (= s status))
                    [:satisfied :optimal-solution :all-solutions]))))


(defn- parse-json-output
  "Parse JSON output from 'minizinc --output-mode json'"
  [stdout]
  # Count JSON objects to determine if multiple solutions
  (var json-count 0)
  (var pos 0)
  (forever
    (def json-start (string/find "{" stdout pos))
    (when (nil? json-start) (break))
    (++ json-count)
    (def json-end (string/find "}" stdout json-start))
    (when (nil? json-end) (break))
    (set pos (+ json-end 1)))
  (if (> json-count 1)
    # Multiple solutions
    (do
      (def solutions @[])
      (set pos 0)
      (forever
        (def json-start (string/find "{" stdout pos))
        (def json-end (string/find "}" stdout pos))
        (when (nil? json-start) (break))
        (def json-str (string/slice stdout json-start (+ json-end 1)))
        (def parsed (json/decode json-str))
        (array/push solutions parsed)
        (set pos (+ json-end 1)))
      @{:status :all-solutions
        :solution (when (> (length solutions) 0) (first solutions))
        :solutions solutions
        :statistics @{}
        :objective nil
        :raw stdout})
    # Single solution
    (do
      (def json-start (string/find "{" stdout))
      (def json-end (string/find "}" stdout))
      (unless json-start
        (error "no JSON output found"))
      (def json-str (string/slice stdout json-start (+ json-end 1)))
      (def parsed (json/decode json-str))
      # Check if this is optimal (has _objective)
      (def status (if (get parsed "_objective")
                   :optimal-solution
                   :satisfied))
      @{:status status
        :solution parsed
        :statistics @{}
        :objective (get parsed "_objective" nil)
        :raw stdout})))


(defn- parse-dzn-line
  "Parse a single DZN key: value line."
  [line]
  (def trimmed (string/trim line))
  (when (and (string/has-suffix? trimmed ";") (> (length trimmed) 1))
    (def without-semi (string/trim (string/slice trimmed 0 -2)))
    (def colon-idx (string/find "=" without-semi))
    (when colon-idx
      (def key (string/trim (string/slice without-semi 0 colon-idx)))
      (def val-str (string/trim (string/slice without-semi (+ colon-idx 1))))
      [key val-str])))


(defn- parse-dzn-value
  "Parse a DZN value string to a Janet value."
  [val-str]
  (def trimmed (string/trim val-str))
  (cond
    (= trimmed "true") true
    (= trimmed "false") false
    (= trimmed "undefined") nil
    (= trimmed "") nil
    (peg/match ~(sequence (opt "-") (some (range "0" "9")) (opt (sequence "." (some (range "0" "9"))))) trimmed)
      (let [n (scan-number trimmed)]
        (if n n trimmed))
    (peg/match ~(sequence "\"" (any (not "\"")) "\"") trimmed)
      (string/slice trimmed 1 -2)
    (= (first trimmed) "[")
      (do
        (def inner (string/slice trimmed 1 -2))
        (if (= (string/trim inner) "")
          @[]
          (map parse-dzn-value (string/split "," inner))))
    (= (first trimmed) "{")
      (do
        (def inner (string/slice trimmed 1 -2))
        (if (= (string/trim inner) "")
          @{}
          (do
            (def tbl @{})
            (each v (string/split "," inner)
              (put tbl (string/trim v) true))
            tbl)))
    trimmed))


(defn parse-dzn-output
  "Parse DZN format output (key: value;)."
  [stdout]
  (def solution @{})
  (def stats @{})
  (def lines (string/split "\n" stdout))
  (var status :satisfied)
  (each line lines
    (def trimmed (string/trim line))
    # Handle status lines like =====UNSATISFIABLE=====
    (cond
      (= trimmed "=====UNSATISFIABLE=====")
        (set status :unsatisfiable)
      (= trimmed "=====UNSATISFIABLE===== ")
        (set status :unsatisfiable)
      (= trimmed "=====UNKNOWN=====")
        (set status :unknown)
      (= trimmed "=====UNKNOWN===== ")
        (set status :unknown)
      (do
        (def parsed (parse-dzn-line line))
        (when parsed
          (def key (parsed 0))
          (def val-str (parsed 1))
          (cond
            (= key "_objective")
              (do
                (put stats :objective (parse-dzn-value val-str))
                (put solution :objective (parse-dzn-value val-str)))
            (peg/match ~(sequence "%" (any " ")) key)
              nil
            (do
              (put solution key (parse-dzn-value val-str))))))))
  @{:status status
    :solution solution
    :statistics stats
    :objective (get stats :objective nil)
    :raw stdout})


(defn parse-output
  "Parse minizinc output. Auto-detects JSON or DZN format."
  [stdout]
  (def trimmed (string/trim stdout))
  (if (or (= "{" (string/slice trimmed 0 1))
          (peg/match ~(sequence "{" (any (not "}"))) trimmed))
    (parse-json-output stdout)
    (parse-dzn-output stdout)))
