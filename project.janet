(declare-project
  :name "minizinc"
  :description "MiniZinc constraint solver bindings."
  :author "nik gaffney <nik@fo.am>"
  :url "https://codeberg.org/zzkt/minizinc-janet"
  :license "GPL-3.0-or-later"
  :version "0.1.1"
  :dependencies @[])

(declare-source
  :prefix "minizinc"
  :source ["minizinc/init.janet"
           "minizinc/driver.janet"
           "minizinc/solver.janet"
           "minizinc/model.janet"
           "minizinc/instance.janet"
           "minizinc/result.janet"
           "minizinc/params.janet"
           "minizinc/utils.janet"])
