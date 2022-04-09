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
(def train (b->ir (slurp (str mch-dir "Train.mch"))))
(def demo (b->ir (slurp (str mch-dir "demo.mch"))))

(def test-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :keep-statespace true
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
    (is (= 11 (:states (model-check (b->ir (ir->b encoded-scheduler))))) "The transformed scheduler has the correct number of states.")))

(deftest numbers-test
  (is (= (ir->b numbers) (ir->b (boolencode numbers))) "A Machine without any Sets should not change in any Way."))

;; This Test takes very long to complete because of ir->b on a large IR. Commented out for better test performance.
;; (deftest train-machine-test
;;   (let [train-encoded (boolencode train)]
;;     (is (map? train-encoded) "The translation completed.")
;;     (is (string? (ir->b train-encoded)) "The Train machine can be translated back to B string")))

(deftest demo-machine-test
  (let [demo-encoded (boolencode demo)
        mc-demo (model-check demo)
        mc-enc (model-check (b->ir (ir->b demo-encoded)))]
    (is (= (:states mc-enc) (:states mc-demo)))
    (is (= (:transitions mc-enc) (:transitions mc-demo)))))
