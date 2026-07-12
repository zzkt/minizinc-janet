# -*- mode: janet; -*-
# minizinc: instances

(import ./driver :as driver)
(import ./solver :as solver)
(import ./model :as model)
(import ./params :as params)
(import ./result :as result)


(defn create
  "Create a new instance binding a solver and model."
  [sol mdl]
  @{:solver sol
    :model mdl
    :params @{}
    :extra-code @[]})

(defn set-param
  "Set parameter values on the instance. Takes key-value pairs.
  (set-param inst :n 4 :m 3)"
  [inst & kvs]
  (var i 0)
  (while (< i (length kvs))
    (put (inst :params) (kvs i) (kvs (+ i 1)))
    (+= i 2))
  nil)

(defn add-string
  "Add extra MiniZinc code to the instance."
  [inst code-str]
  (array/push (inst :extra-code) code-str)
  nil)

(defn- write-temp-dzn
  "Write DZN data to a temporary file and return the path."
  [inst]
  (def dzn-content (params/write-dzn (inst :params)))
  (if (= "" dzn-content)
    nil
    (do
      (def tmp-path (string "/tmp/mzn_data_" (string (math/random) ".dzn")))
      (spit tmp-path dzn-content)
      tmp-path)))

(defn- write-temp-mzn
  "Write inline code to a temporary .mzn file."
  [inst]
  (def extra (inst :extra-code))
  (if (= 0 (length extra))
    nil
    (do
      (def buf @"")
      (each c extra
        (buffer/push-string buf c)
        (buffer/push-string buf "\n"))
      (def tmp-path (string "/tmp/mzn_inline_" (string (math/random) ".mzn")))
      (spit tmp-path (string buf))
      tmp-path)))

(defn- build-args
  "Build the minizinc command arguments."
  [inst opts]
  (def sol (inst :solver))
  (def mdl (inst :model))
  (def args @["--solver" (solver/solver-id sol)])
  (when (get opts :all-solutions false)
    (array/push args "-a"))
  (when (get opts :free-search false)
    (array/push args "-f"))
  (when-let [tl (get opts :time-limit nil)]
    (array/push args "-t")
    (array/push args (string tl)))
  (when-let [n (get opts :nr-solutions nil)]
    (array/push args "-n")
    (array/push args (string n)))
  (when-let [p (get opts :processes nil)]
    (array/push args "-p")
    (array/push args (string p)))
  (when-let [r (get opts :random-seed nil)]
    (array/push args "-r")
    (array/push args (string r)))
  (when-let [ol (get opts :optimisation-level nil)]
    (array/push args (string "--optimisation-level" ol)))
  (when (get opts :intermediate-solutions false)
    (array/push args "-i"))
  (array/push args "--output-mode")
  (array/push args "json")
  (array/push args "--output-time")
  (array/push args "--output-objective")
  (array/push args "--output-output-item")
  # return the arg array
  args)

(defn solve
  "Solve the instance. Returns a result table."
  [inst &opt opts]
  (default opts {})
  (def dzn-path (write-temp-dzn inst))
  (def mzn-path (write-temp-mzn inst))
  (def args (build-args inst opts))
  (when dzn-path
    (array/push args dzn-path))
  (when mzn-path
    (array/push args mzn-path))
  (def mdl (inst :model))
  (def model-files (model/files mdl))
  (var inline-mzn nil)
  (if (> (length model-files) 0)
    # Has model file(s) add them as args
    (each f model-files
      (array/push args f))
    # No model file(s) write code to a .mzn file
    (do
      (def code (model/code mdl))
      (when (> (length code) 0)
        (set inline-mzn (string "/tmp/mzn_model_" (string (math/random) ".mzn")))
        (spit inline-mzn code)
        (array/push args inline-mzn))))
  # Results
  (def res (driver/run args))
  # Cleanup temp files (ignore errors if already deleted)
  (when dzn-path
    (try (os/rm dzn-path) ([_] nil)))
  (when mzn-path
    (try (os/rm mzn-path) ([_] nil)))
  (when inline-mzn
    (try (os/rm inline-mzn) ([_] nil)))
  (unless (= 0 (res :exit))
    (error (string "minizinc failed (exit " (res :exit) "): " (res :stderr))))
  (result/parse-output (res :stdout)))

(defn branch
  "Create a child instance for incremental solving.
  Changes to the child do not affect the parent."
  [inst]
  (def child (create (inst :solver) (inst :model)))
  (put child :params (table/clone (inst :params)))
  (put child :extra-code (array/slice (inst :extra-code)))
  child)
