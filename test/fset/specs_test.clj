(ns fset.specs-test
  (:require
   [clojure.spec.alpha :as spec]
   [fset.specs :refer :all]
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [b->ir]]))

(def empty-ir (b->ir (slurp "resources/test/Empty.mch")))
(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))
(def items-ir (b->ir (slurp "resources/test/Items.mch")))
(def train-ir (b->ir (slurp "resources/test/Train.mch")))


(deftest lisb-ir-spec-test
  (testing "IR read via b->ir should be valid."
    (is (spec/valid? :lisb/ir empty-ir))
    (is (spec/valid? :lisb/ir scheduler-ir))
    (is (spec/valid? :lisb/ir items-ir))
    (is (spec/valid? :lisb/ir train-ir)))
  (testing "Not everything is a valid IR"
    (is (not (spec/valid? :lisb/ir {})))
    (is (not (spec/valid? :lisb/ir {:tag :maösldkfj :clauses #{}})))
    (is (not (spec/valid? :lisb/ir {:tag :machine :clauses '() :name 1})))))
