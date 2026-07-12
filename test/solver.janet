(import spork/test)
(import ../minizinc/solver :as solver)

(test/start-suite 1)

(test/assert-no-error "solver lookup gecode"
  (do (def s (solver/lookup "gecode"))
    (assert s "found gecode")
    (assert (= (get s "name") "Gecode") "name is Gecode")))

(test/assert-no-error "solver lookup by id suffix"
  (do (def s (solver/lookup "org.gecode.gecode"))
    (assert s "found gecode by full id")))

(test/assert-no-error "solver supports-all-solutions?"
  (do (def s (solver/lookup "gecode"))
    (assert (solver/supports-all-solutions? s) "gecode supports -a")))

(test/assert-no-error "solver make"
  (do (def s (solver/make "TestSolver" "1.0" "com.test.solver"))
    (assert (= (get s :name) "TestSolver") "custom solver name")))

(test/end-suite)
