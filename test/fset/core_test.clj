(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.pprint :refer [pprint]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.core :refer [boolencode unroll-predicate set->bitvector init-db]]))


(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))
(init-db scheduler-ir)

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-ir (boolencode scheduler-ir)))))

(deftest valid-B-machine-test
  (testing "After the transformation the IR can be translated into a B machine."
    (is (string? (ir->b (boolencode scheduler-ir))))))
