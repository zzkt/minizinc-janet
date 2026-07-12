# -*- mode: janet; -*-
# minizinc: solvers

(import ./driver :as driver)


(defn lookup
  "Look up a solver by tag, id, or name. Returns solver table.
  Searches by: full id, id ending, name, then tags."
  [tag]
  (def solvers (driver/solvers-json))
  (var found nil)
  (each s solvers
    (when (nil? found)
      (def id (get s "id" ""))
      (def name (get s "name" ""))
      (def tags (get s "tags" @[]))
      (cond
        (= id tag) (set found s)
        (string/has-suffix? tag id) (set found s)
        (= name tag) (set found s)
        (any? (map (fn [t] (= t tag)) tags)) (set found s))))
  (unless found
    (error (string "solver not found: " tag)))
  found)


(defn make
  "Create a custom solver configuration table."
  [name version id &opt executable]
  @{:name name
    :version version
    :id id
    :executable executable
    :tags @[]
    :stdFlags @[]
    :extraFlags @[]
    :needsSolns2Out false
    :supportsMzn false})


(defn solver-id
  "Get the solver identifier string for CLI use."
  [solver]
  (get solver "id"))

(defn std-flags
  "Get the standard flags supported by a solver."
  [solver]
  (get solver "stdFlags" @[]))


(defn supports-all-solutions?
  "Check if solver supports the -a flag for all solutions."
  [solver]
  (def flags (std-flags solver))
  (any? (map (fn [f] (= f "-a")) flags)))
