(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b]]
   [fset.test-machines :refer [empty-ir scheduler scheduler-transformed numbers]]
   [fset.core :refer [boolencode]]))


(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler (boolencode scheduler)))))

(deftest valid-B-machine-test
    (is (string? (ir->b (boolencode scheduler))) "The IR can be translated into a B machine.")
    (is (= scheduler-transformed (boolencode scheduler)) "The scheduler is correctly translated."))

(deftest numbers-test
  (is (= numbers (boolencode numbers)) "A Machine without any Sets should not change in any Way."))
