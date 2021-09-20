(ns fset.util-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.util :refer [get-nodes-by-tag update-nodes-by-tag]]))

(def empty-mch-ir
  {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses nil})

(deftest get-machine-header-test
  (is (= (get-nodes-by-tag :machine-header empty-mch-ir)
         [{:tag :machine-header, :name :Empty, :parameters []}])))


(deftest update-machine-header-test
  (is (= (update-nodes-by-tag :machine-header #(assoc % :name :Full) empty-mch-ir)
         {:tag :machine, :variant {:tag :machine-variant}, :header
          {:tag :machine-header, :name :Full,
           :parameters []},
          :clauses nil})))
