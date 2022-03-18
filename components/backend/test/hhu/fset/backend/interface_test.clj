(ns hhu.fset.backend.interface-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [hhu.fset.backend.interface :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))

(def test-config
  {:max-unroll-size 200
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

(deftest core-interface-test
  (b/setup-backend scheduler test-config)
  (is (= '({:name :activePID1, :elem :PID1, :var :active}
           {:name :activePID2, :elem :PID2, :var :active}
           {:name :activePID3, :elem :PID3, :var :active}
           {:name :readyPID1, :elem :PID1, :var :ready}
           {:name :readyPID2, :elem :PID2, :var :ready}
           {:name :readyPID3, :elem :PID3, :var :ready}
           {:name :waitingPID1, :elem :PID1, :var :waiting}
           {:name :waitingPID2, :elem :PID2, :var :waiting}
           {:name :waitingPID3, :elem :PID3, :var :waiting})
         (b/get-all-bools)))
  (is (= [[:PID1 :PID2 :PID3]] (b/get-type-elem-matrix :active)))
  (is (= [[{:tag :maplet, :left :PID1, :right :PID1}
           {:tag :maplet, :left :PID1, :right :PID2}
           {:tag :maplet, :left :PID1, :right :PID3}]
          [{:tag :maplet, :left :PID2, :right :PID1}
           {:tag :maplet, :left :PID2, :right :PID2}
           {:tag :maplet, :left :PID2, :right :PID3}]
          [{:tag :maplet, :left :PID3, :right :PID1}
           {:tag :maplet, :left :PID3, :right :PID2}
           {:tag :maplet, :left :PID3, :right :PID3}]]
         (b/get-type-elem-matrix {:tag :maplet :left :PID1 :right :PID2})))
  (is (b/type? :PID))
  (is (not (b/type? :active)))
  (is (= '({:name :activePID1, :elem :PID1, :var :active}
           {:name :activePID2, :elem :PID2, :var :active}
           {:name :activePID3, :elem :PID3, :var :active})
         (b/unroll-variable :active)))
  (is (= '(:PID1 :PID2 :PID3) (b/get-type-elems :PID1))))

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
  (is (b/contains-vars? (b->ir "#PREDICATEactive={}")))
  (is (not (b/contains-vars? "#PREDICATEPID1:PID")))
  (is (b/variable? :active))
  (is (b/carrier? :PID))
  (is (= "PID1:PID & PID2:PID" (ir->b (b/apply-binding (b->ir "#PREDICATEx:PID & y:PID") [[:x :PID1] [:y :PID2]]))))
  (is (not (b/constant? :active)))
  (is (b/set-element? :PID1))
  (is (b/simple-tuple? (b->ir "#EXPRESSIONPID1|->PID2")))
  (is :activePID1 (b/create-boolname :active :PID1))
  (is (= 5 (b/num-ops))))
