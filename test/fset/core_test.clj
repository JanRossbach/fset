(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.core :as fset]))


(def empty-ir {:tag :machine, :clauses nil, :name :Empty})

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (:ir (fset/boolencode 10 3 true empty-ir :nil))))))

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-ir (fset/boolencode 10 3 true scheduler-ir :PID)))))

(deftest valid-B-machine-test
  (testing "After the transformation the IR can be translated into a B machine."
    (is (string? (ir->b (:ir (fset/boolencode 10 3 true scheduler-ir :PID)))))))
