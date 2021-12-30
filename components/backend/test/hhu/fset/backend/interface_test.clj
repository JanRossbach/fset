(ns hhu.fset.backend.interface-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [hhu.fset.backend.interface :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))

(def test-config
  {:max-unroll-size 200
   :eval-constants true
   :unroll-invariant true
   :simplify-result true
   :deff-set-size 2
   :logging false
   :excluded-vars #{:TRK :rsrtbl}})

(def scheduler (b->ir (slurp "components/backend/resources/backend/scheduler.mch")))

(deftest setup-backend-test
  (let [ret-val (b/setup-backend scheduler test-config)]
    (is (string? (ir->b ret-val)) "The return value can be turned back into a B machine")
    (is (= scheduler ret-val) "For the scheduler the return value should not change")
    (testing "The backend atom should contain the correct values"
      (is (= scheduler (:ir @b/db)))
      (is (= de.prob.statespace.StateSpace (type (:ss @b/db))))
      (is (= test-config (:cfg @b/db))))))

(deftest lisb-interface-test
  (b/setup-backend scheduler test-config)
  (is (map? (b/model-check scheduler)))
  (is (= "POW(PID)" (b/get-type :active)))
  (is (b/intexpr? 1))
  (is (b/setexpr? :ready)))

(deftest specter-util-interface-test
  (b/setup-backend scheduler test-config)
  (is (= [:active :ready :waiting] (b/get-vars)))
  (is (empty? (b/get-constants)))
  (is (b/variable? :active))
  (is (b/carrier? :PID))
  (is (not (b/constant? :active)))
  (is (b/set-element? :PID1))
  (is :activePID1 (b/create-boolname :active :PID1)))
