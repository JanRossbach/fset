(ns fset.extract-test
  (:require
   [fset.extract :as ex]
   [lisb.translation.util :refer [lisb->ir]]
   [clojure.test :refer [deftest is testing]]))


(def example-ir {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :fun_extract, :parameters []}, :clauses '({:tag :sets, :set-definitions ({:tag :enumerated-set, :identifier :A, :elements (:VAR1 :VAR2)})} {:tag :variables, :identifiers (:p)} {:tag :invariants, :predicate {:tag :member, :element :p, :set {:tag :total-fn, :sets (:A {:tag :interval, :from 5, :to 6})}}} {:tag :init, :substitution {:tag :assign, :identifiers (:p), :values (#{[:VAR1 5] [:VAR2 6]})}})})


(deftest example-test
  (testing "The translated example is equal to the target string"
    (is (not= example-ir (ex/extract example-ir '(:p) (lisb->ir '(range 5 6)))))))
