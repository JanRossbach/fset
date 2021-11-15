(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.core :refer [boolencode]]))


(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))
(def scheduler-transformed-ir (read-string (slurp "resources/test/scheduler-ir.edn")))

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-ir (boolencode scheduler-ir)))))

(deftest valid-B-machine-test
    (is (string? (ir->b (boolencode scheduler-ir))) "The IR can be translated into a B machine.")
    (is (= scheduler-transformed-ir (boolencode scheduler-ir)) "The scheduler is correctly translated."))
