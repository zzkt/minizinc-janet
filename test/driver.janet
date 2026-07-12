(import spork/test)
(import ../minizinc/driver :as driver)

(test/start-suite 0)

(test/assert-no-error "find driver"
  (do (def p (driver/find))
    (assert p "minizinc executable found")))

(test/assert-no-error "get path"
  (do (def p (driver/get-path))
    (assert p "get-path returns path")))

(test/assert-no-error "version returns tuple"
  (do (def v (driver/version))
    (assert (= (type v) :tuple) "version is tuple")
    (assert (>= (length v) 2) "version has >= 2 parts")))

(test/assert-no-error "solvers-json returns array"
  (do (def s (driver/solvers-json))
    (assert (= (type s) :array) "solvers is array")
    (assert (> (length s) 0) "has at least one solver")))

(test/end-suite)
