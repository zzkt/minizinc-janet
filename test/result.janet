(import spork/test)
(import ../minizinc/result :as result)

(test/start-suite 4)

(test/assert-no-error "parse-status"
  (do (assert (= (result/parse-status "SATISFIED") :satisfied) "SATISFIED")
    (assert (= (result/parse-status "OPTIMAL_SOLUTION") :optimal-solution) "OPTIMAL")
    (assert (= (result/parse-status "UNSATISFIABLE") :unsatisfiable) "UNSATISFIABLE")))

(test/assert-no-error "status-has-solution?"
  (do (assert (result/status-has-solution? :satisfied) "satisfied solution")
    (assert (result/status-has-solution? :optimal-solution) "optimal solution")
    (test/assert-not (result/status-has-solution? :unsatisfiable) "unsatisfiable, no solution")))

(test/end-suite)
