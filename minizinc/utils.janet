# -*- mode: janet; -*-
# utilities: solver and option listing

(import ./driver :as driver)

(defn list-solvers
  "List all installed solvers. Returns array of {:name :version :id :tags} tables."
  []
  (def json-solvers (driver/solvers-json))
  (def solvers @[])
  (each s json-solvers
    (array/push solvers @{
      :name (get s "name")
      :version (get s "version")
      :id (get s "id")
      :tags (get s "tags" @[])
      :stdFlags (get s "stdFlags" @[])
      :extraFlags (get s "extraFlags" @[])
    }))
  solvers)


(defn solver-options
  "Get options for all solvers. Returns array of {:flag :description :solver} tables."
  []
  (def json-solvers (driver/solvers-json))
  (def options @[])
  (each s json-solvers
    (def name (get s "name"))
    # Standard flags
    (each flag (get s "stdFlags" @[])
      (array/push options @{
        :flag flag
        :description (string name " standard flag")
        :solver name
      }))
    # Extra flags with descriptions
    (each ef (get s "extraFlags" @[])
      (array/push options @{
        :flag (ef 0)
        :description (ef 1)
        :solver name
      }))
    options)
  options)


(defn print-solver-table
  "Print a formatted table of all installed solvers."
  []
  (def solvers (list-solvers))
  (printf "Installed Solvers (%d):\n" (length solvers))
  (each s solvers
    (printf "  %-20s %-12s %s" (s :name) (s :version) (s :id))
    (when (> (length (s :tags)) 0)
      (printf "    Tags: %s" (string/join (s :tags) ", ")))))


(defn print-solver-options
  "Print solver options grouped by solver."
  []
  (def solvers (list-solvers))
  (printf "Solver Options:\n")
  (each s solvers
    (printf "  %s (%s):" (s :name) (s :version))
    (printf "    Standard flags: %s" (string/join (s :stdFlags) " "))
    (when (> (length (s :extraFlags)) 0)
      (printf "    Extra flags:")
      (each ef (s :extraFlags)
        (printf "      %-24s %s (default: %s)" (ef 0) (ef 1) (ef 3))))))
