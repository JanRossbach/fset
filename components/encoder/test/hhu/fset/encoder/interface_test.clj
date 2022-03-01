(ns hhu.fset.encoder.interface-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [hhu.fset.backend.interface :refer [model-check]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [hhu.fset.encoder.interface :as fset]))

(def mch-dir "components/encoder/resources/encoder/test/")

(def empty-ir {:tag :machine, :machine-clauses '(), :name :empty, :args []})

(def scheduler (b->ir (slurp (str mch-dir "scheduler.mch"))))
(def numbers (b->ir (slurp (str mch-dir "Numbers.mch"))))
(def banking (b->ir (slurp (str mch-dir "Banking.mch"))))
(def train (b->ir (slurp (str mch-dir "Train.mch"))))
(def test-ir (b->ir (slurp (str mch-dir "Test.mch"))))


(def test-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{:TRK :rsrtbl}})

(defn boolencode
  [ir]
  (fset/boolencode ir test-config))

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (boolencode empty-ir)))))

(deftest scheduler-machine-test
  (let [encoded-scheduler (boolencode scheduler)]
    (is (not= scheduler encoded-scheduler) "The scheduler machine should be changed in some way")
    (is (string? (ir->b encoded-scheduler)) "The IR can be translated into a B machine.")
    (is (= 36 (:states (model-check (b->ir (ir->b encoded-scheduler))))) "The transformed scheduler has the correct number of states.")))

(deftest numbers-test
  (is (= numbers (boolencode numbers)) "A Machine without any Sets should not change in any Way."))

;; This Test takes very long to complete. Comment out for better test performance.
(deftest train-machine-test
  (let [train-encoded (boolencode train)]
    (is (map? train-encoded) "The translation completed.")
    (is (string? (ir->b train-encoded)) "The Train machine can be translated back to B string")))
