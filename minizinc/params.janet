# -*- mode: janet; -*-
# minizinc: parameters and serialization

(var serialize nil)
(var serialize-array nil)
(var serialize-matrix nil)
(var serialize-set nil)

(set serialize
  (fn [val]
    (cond
      (nil? val) (error "cannot serialize nil to DZN")
      (= (type val) :boolean) (if val "true" "false")
      (= (type val) :integer) (string val)
      (= (type val) :number) (string val)
      (= (type val) :string) (string "\"" val "\"")
      (= (type val) :tuple) (serialize-array (tuple/slice val))
      (= (type val) :array) (serialize-array val)
      (= (type val) :range) (string (first val) ".." (- (last val) 1))
      (= (type val) :table) (serialize-set val)
      (= (type val) :struct) (serialize-set val)
      (error (string "unsupported DZN type: " (type val))))))

(set serialize-array
  (fn [arr]
    (def n (length arr))
    (if (= n 0)
      "[]"
      (do
        (def first-val (first arr))
        (def is-2d (or (= (type first-val) :array)
                       (= (type first-val) :tuple)))
        (if is-2d
          (serialize-matrix arr)
          (string "[" (string/join (map serialize arr) ", ") "]"))))))

(set serialize-matrix
  (fn [matrix]
    (def nrows (length matrix))
    (if (= nrows 0)
      "array2d(1..0, 1..0, [])"
      (do
        (def ncols (length (first matrix)))
        (def flat (map serialize (mapcat identity matrix)))
        (string "array2d(1.." nrows ", 1.." ncols ", ["
                (string/join flat ", ") "])")))))

(set serialize-set
  (fn [tbl]
    (def vals (if (dictionary? tbl)
                (values tbl)
                (map identity tbl)))
    (if (= (length vals) 0)
      "{}"
      (string "{" (string/join (map serialize vals) ", ") "}"))))

(defn write-dzn
  "Write parameters to a DZN string. Takes a table of key-value pairs."
  [params]
  (def buf @"")
  (eachp [k v] params
    (buffer/push-string buf (string k " = " (serialize v) ";\n")))
  (string buf))

(defn serialize-value
  "Serialize a single Janet value to DZN."
  [val]
  (serialize val))
