(declare-project
  :name "minizinc"
  :description "MiniZinc constraint solver bindings."
  :author "nik gaffney <nik@fo.am>"
  :url "https://codeberg.org/zzkt/minizinc-janet"
  :license "EUPL-1.2"
  :version "0.1.3"
  :dependencies @["spork"]) # for spork/json

(declare-source
  :prefix "minizinc"
  :source ["minizinc/init.janet"
           "minizinc/driver.janet"
           "minizinc/solver.janet"
           "minizinc/model.janet"
           "minizinc/instance.janet"
           "minizinc/result.janet"
           "minizinc/serializer.janet"
           "minizinc/utils.janet"])
