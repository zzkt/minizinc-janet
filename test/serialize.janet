# -*- mode: janet; -*-
# test/params: parametric?

(import spork/test)
(import ../minizinc/serializer :prefix "")

(test/start-suite 3)

(test/assert-no-error "serialize int"
  (do (assert (= (serialize 42) "42")
              "int serialization")))

(test/assert-no-error "serialize float"
  (do (assert (= (serialize 3.14) "3.14")
              "float serialization")))

(test/assert-no-error "serialize string"
  (do (assert (= (serialize "hello") "\"hello\"")
              "string serialization")))

(test/assert-no-error "serialize bool"
  (do (assert (= (serialize true) "true") "bool true")
    (assert (= (serialize false) "false") "bool false")))

(test/assert-no-error "serialize array 1D"
  (do (assert (= (serialize @[1 2 3]) "[1, 2, 3]")
              "1D array")))

(test/assert-no-error "serialize matrix 2D"
  (do (def mat @[@[1 2] @[3 4]])
    (def res (serialize mat))
    (assert (string/has-prefix? "array2d" res)
            "matrix has array2d prefix")))

(test/assert-no-error "serialize range"
  (do (assert (= (serialize (range 1 11))
                 "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]")
              "range serialization")))

(test/assert-no-error "serialize set"
  (do (def s @{1 true 2 true 3 true})
    (def res (serialize s))
    (assert (string/has-prefix? "{" res) "set starts with {")))

(test/assert-no-error "write-dzn"
  (do (def dzn (write-dzn {"n" 8 "m" 3}))
    (assert (string/has-suffix? "\n" dzn) "dzn ends with newline")
    (assert (> (length dzn) 0) "dzn not empty")))

(test/assert-no-error "write-dzn with matrix"
  (do (def dzn (write-dzn {"grid" @[@[1 2 3] @[4 5 6] @[7 8 9]]}))
    (assert (string/has-suffix? "\n" dzn)
            "dzn with matrix ends with newline")))

(test/end-suite)
