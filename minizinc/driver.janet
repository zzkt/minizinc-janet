# -*- mode: janet; -*-
# minizinc/driver: paths and config

(import spork/json)

(var *minizinc-path* nil)

(defn find-minizinc
  "Find the minizinc executable. Return path as string or nil."
  []
  (let [path-str (os/getenv "PATH")
        sep (if (= :windows (os/which :windows)) ";" ":")
        dirs (string/split sep path-str)]
    (var found nil)
    (each dir dirs
      (when (not found)
        (def candidate (string dir "/minizinc"))
        (when (= :file (get (os/stat candidate) :mode))
          (set found candidate))))
    # If not found in PATH, check common locations
    (unless found
      (def candidates
        [ "/opt/homebrew/bin/minizinc" "/usr/local/bin/minizinc"
         (string (os/getenv "HOME") "/MiniZincIDE.app/Contents/Resources/minizinc")])
      (each c candidates
        (when (and (not found) (= :file (get (os/stat c) :mode)))
          (set found c))))
    found))

(defn get-path
  "Get the current minizinc path."
  []
  (when (nil? *minizinc-path*)
    (set *minizinc-path* (find-minizinc)))
  (unless *minizinc-path*
    (error "minizinc executable not found on system"))
  *minizinc-path*)

(defn run-minizinc
  "Run minizinc with given args. Returns {:stdout :stderr :exit}."
  [args]
  (let [mzn-bin (get-path)
        full-args (array/concat @[mzn-bin "--allow-multiple-assignments"] args)
        _stdout @""
        _stderr @""
        proc (os/spawn full-args :p {:out :pipe :err :pipe})]
    (ev/read (proc :out) :all _stdout)
    (ev/read (proc :err) :all _stderr)
    (os/proc-wait proc)
    (os/proc-close proc)
    {:stdout (string _stdout)
     :stderr (string _stderr)
     :exit (proc :return-code)}))

(defn version
  "Get minizinc version as a tuple (major minor patch)."
  []
  (def res (run-minizinc ["--version"]))
  (def first-line (first (string/split "\n" (res :stdout))))
  # Find "version" keyword and extract numbers after it
  (def idx (string/find "version" first-line))
  (unless idx (error "could not parse minizinc version"))
  (def after (string/slice first-line (+ idx 7)))
  (def ver-peg ~(sequence (any " ") (capture (some :d))
                          "." (capture (some :d))
                          "." (capture (some :d))))
  (def matches (peg/match ver-peg after))
  (if matches
    (tuple/slice (map scan-number matches))
    (error "could not parse minizinc version")))


(defn solvers-json
  "Get available solvers as parsed JSON array of tables."
  []
  (def res (run-minizinc ["--solvers-json"]))
  (unless (= 0 (res :exit))
    (error (string "minizinc --solvers-json failed: " (res :stderr))))
  (json/decode (res :stdout)))


(defn analyse-model
  "Analyse a model file. Returns parsed JSON with input/output info."
  [file-path]
  (def res (run-minizinc ["--model-interface-only" file-path]))
  (unless (= 0 (res :exit))
    (error (string "minizinc model analysis failed: " (res :stderr))))
  (json/decode (res :stdout)))
