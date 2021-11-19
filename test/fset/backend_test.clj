(ns fset.backend-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [lisb.translation.util :refer [b->ir]]
   [lisb.translation.lisb2ir :refer [b= bor]]
   [fset.backend :as b]))

;; Tests on the scheduler

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))

(b/setup-backend scheduler-ir)

(deftest carrier?-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (b/carrier? :PID))
  (is (not (b/carrier? :active)))
  (is (not (b/carrier? 1))))

(deftest create-boolname-test-scheduler
  (b/setup-backend scheduler-ir)
  (are [x y] (= x y)
    :activePID1 (b/create-boolname :active :PID1)
    :activePID1PID2 (b/create-boolname :active [:PID1 :PID2])))

(deftest set-element?-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (b/set-element? :PID1))
  (is (not (b/set-element? :active)))
  (is (not (b/set-element? :PID)))
  (is (not (b/set-element? 1))))

(deftest unrollable-var?-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (b/unrollable-var? :active))
  (is (not (b/unrollable-var? :PID)))
  (is (not (b/unrollable-var? :swap))))

(deftest variable?-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (b/variable? :active))
  (is (b/variable? :ready))
  (is (b/variable? :waiting))
  (is (not (b/variable? :PID)))
  (is (not (b/variable? :PID1)))
  (is (not (b/variable? 1)))
  (is (not (b/variable? false))))

(deftest enumerable?-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (b/finite? :active))
  (is (b/finite? {:tag :union :sets '(:active :ready)})))

(deftest unroll-variable-test-scheduler
  (b/setup-backend scheduler-ir)
  (are [x y] (= x y)
    '(:activePID1 :activePID2 :activePID3) (b/unroll-variable :active)
    '(:readyPID1 :readyPID2 :readyPID3) (b/unroll-variable :ready)
    '(:waitingPID1 :waitingPID2 :waitingPID3) (b/unroll-variable :waiting)))

(deftest get-all-elems-from-elem-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID1)))
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID2)))
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID3))))

(deftest pick-bool-var-test-scheduler
  (b/setup-backend scheduler-ir)
  (is (= (list (b= :activePID1 :TRUE)) (b/pick-bool-var (list (b= :activePID1 :TRUE) (b= :activePID2 :TRUE) (b= :activePID3 :FALSE)) :PID1)))
  (is (= (list (bor (b= :activePID2 :TRUE) (b= :readyPID2 :TRUE))) (b/pick-bool-var (list (b= :TRUE :FALSE) (bor (b= :activePID2 :TRUE) (b= :readyPID2 :TRUE)) (b= :activePID3 :FALSE)) :PID2)))
  (is (= (list (b= :activePID3 :FALSE)) (b/pick-bool-var (list (b= :activePID1 :TRUE) (b= :activePID2 :TRUE) (b= :activePID3 :FALSE)) :PID3))))

(deftest get-op-combinations-test-scheduler
  (testing "The scheduler Operation Bindings are calculated correctly."
    (b/setup-backend scheduler-ir)
    (is (= [] (b/op->bindings (b/get-operation :nr_ready))))
    (is (= [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]] (b/op->bindings (b/get-operation :new))))
    (is (= [[[:rr :PID1]] [[:rr :PID2]] [[:rr :PID3]]] (b/op->bindings (b/get-operation :ready))))
    (is (= [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]] (b/op->bindings (b/get-operation :del))))
    (is (= [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]] (b/op->bindings (b/get-operation :swap))))))

;; Tests on the Test machine

(def test-ir (b->ir (slurp "resources/test/Test.mch")))

(deftest set-element?-test-Testmch
  (b/setup-backend test-ir)
  (is (b/set-element? :c1))
  (is (b/set-element? :c2))
  (is (b/set-element? :c3))
  (is (b/set-element? :A1))
  (is (b/set-element? :B1))
  (is (b/set-element? :B2))
  (is (not (b/set-element? :active))))

(b/get-param-elems (b/get-operation :new) :pp)

;; (clojure.pprint/pprint (band {:tag :and, :preds '({:tag :member, :elem :pp, :set :PID} {:tag :member, :elem :pp, :set :active} {:tag :member, :elem :pp, :set {:tag :union, :sets (:ready :waiting)}})}
;;                              (b/get-invars-as-pred)))
