(ns fset.extract-test
  (:require
   [fset.extract :as ex]
   [fset.util :as util]
   [lisb.translation.util :refer [lisb->ir]]
   [clojure.test :refer [deftest is testing]]))

(def example-source-str
"MACHINE fun_extract
SETS
  A = {VAR1, VAR2}
VARIABLES
  p
INVARIANT
  p : A --> 5..6
INITIALISATION  p := {VAR1 |-> 5, VAR2 |-> 6}
END")

(def example-ir {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :fun_extract, :parameters []}, :clauses '({:tag :sets, :set-definitions ({:tag :enumerated-set, :identifier :A, :elements (:VAR1 :VAR2)})} {:tag :variables, :identifiers (:p)} {:tag :invariants, :predicate {:tag :member, :element :p, :set {:tag :total-fn, :sets (:A {:tag :interval, :from 5, :to 6})}}} {:tag :init, :substitution {:tag :assign, :identifiers (:p), :values (#{[:VAR1 5] [:VAR2 6]})}})})

(def example-target-str
"MACHINE fun_extract
SETS A={VAR1,VAR2}
VARIABLES VAR1, VAR2
INVARIANT VAR1:5..6 & VAR2:5..6
INITIALISATION VAR1 := 5 || VAR2 := 6
END")

(def example-target-ir
  {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :fun_extract, :parameters []}, :clauses '({:tag :sets, :set-definitions ({:tag :enumerated-set, :identifier :A, :elements (:VAR1 :VAR2)})} {:tag :variables, :identifiers (:VAR1 :VAR2)} {:tag :invariants, :predicate {:tag :and, :predicates ({:tag :member, :element :VAR1, :set {:tag :interval, :from 5, :to 6}} {:tag :member, :element :VAR2, :set {:tag :interval, :from 5, :to 6}})}} {:tag :init, :substitution {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:VAR1), :values (5)} {:tag :assign, :identifiers (:VAR2), :values (6)})}})})


(deftest example-test
  (testing "The translated example is equal to the target string"
    (is (not= example-ir (ex/extract example-ir '(:p) (lisb->ir '(range 5 6)))))))
