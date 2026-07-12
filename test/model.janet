(import spork/test)
(import ../minizinc/model :as model)

(test/start-suite 2)

(test/assert-no-error "create empty model"
  (do (def m (model/create))
    (assert m "model created")
    (assert (= (length (model/files m)) 0) "no files")
    (assert (= (model/code m) "") "no code")))

(test/assert-no-error "model add-file"
  (do (def m (model/create))
    (model/add-file m "examples/nqueens.mzn")
    (assert (= (length (model/files m)) 1) "one file")
    (assert (= ((model/files m) 0) "examples/nqueens.mzn") "correct path")))

(test/assert-no-error "model add-string"
  (do (def m (model/create))
    (model/add-string m "var 1..10: x;")
    (assert (> (length (model/code m)) 0) "code added")))

(test/assert-no-error "create model from file"
  (do (def m (model/create "examples/nqueens.mzn"))
    (assert (= (length (model/files m)) 1) "one file from constructor")))

(test/end-suite)
