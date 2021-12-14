(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.test-machines :refer [empty-ir scheduler scheduler-transformed numbers train]]
   [fset.core :refer [boolencode]]
   [fset.backend :as b]))

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler (boolencode scheduler)))))

(deftest valid-B-machine-test
  (is (string? (ir->b (boolencode scheduler))) "The IR can be translated into a B machine.")
  (is (= 36 (:states (b/model-check (b->ir (ir->b (boolencode scheduler)))))) "The transformed scheduler has the correct number of states."))

(deftest numbers-test
  (is (= numbers (boolencode numbers)) "A Machine without any Sets should not change in any Way."))

(deftest train-machine-test
  (is (string? (ir->b (boolencode train))) "The Train machine can be translated back to B string"))
