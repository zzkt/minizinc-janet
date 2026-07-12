# -*- mode: janet; -*-
# minizinc: models

(var add-file nil)

(defn create
  "Create a new empty MiniZinc model. Optionally from file paths."
  [&opt files]
  (def m @{:files @[] :code @""})
  (when files
    (each f (if (= (type files) :tuple) files [files])
      (add-file m f)))
  m)

(set add-file
  (fn [model file-path]
    (unless (or (string/has-suffix? ".mzn" file-path)
                (string/has-suffix? ".dzn" file-path)
                (string/has-suffix? ".json" file-path)
                (string/has-suffix? ".fzn" file-path))
      (error (string "unsupported file type: " file-path)))
    (unless (os/stat file-path)
      (error (string "model file not found: " file-path)))
    (array/push (model :files) file-path)
    nil))

(defn add-string
  "Add an inline MiniZinc code string to the model."
  [model code-str]
  (buffer/push-string (model :code) "\n")
  (buffer/push-string (model :code) code-str)
  nil)

(defn has-files?
  "Check if the model has any .mzn files."
  [model]
  (> (length (model :files)) 0))

(defn has-code?
  "Check if the model has inline code."
  [model]
  (> (length (model :code)) 0))

(defn files
  "Get the list of model files."
  [model]
  (model :files))

(defn code
  "Get inline code."
  [model]
  (string (model :code)))
