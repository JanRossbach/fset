(ns fset.backend-test
  (:require
   [clojure.test :refer [deftest testing is are]]
   [lisb.translation.util :refer [b->ir]]
   [fset.backend :as b]))

;; Tests on the scheduler

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))
(b/setup-backend scheduler-ir)

(deftest carrier?-test
  (is (b/carrier? :PID))
  (is (not (b/carrier? :active)))
  (is (not (b/carrier? 1))))

(deftest create-boolname-test
  (are [x y] (= x y)
    :activePID1 (b/create-boolname :active :PID1)
    :activePID1PID2 (b/create-boolname :active [:PID1 :PID2])))

(deftest set-element?-test
  (is (b/set-element? :PID1))
  (is (not (b/set-element? :active)))
  (is (not (b/set-element? :PID)))
  (is (not (b/set-element? 1))))

(deftest unrollable-var?-test
  (is (b/unrollable-var? :active))
  (is (not (b/unrollable-var? :PID)))
  (is (not (b/unrollable-var? :swap))))

(deftest variable?-test
  (is (b/variable? :active))
  (is (b/variable? :ready))
  (is (b/variable? :waiting))
  (is (not (b/variable? :PID)))
  (is (not (b/variable? :PID1)))
  (is (not (b/variable? 1)))
  (is (not (b/variable? false))))

(deftest enumerable?-test
  (is (b/enumerable? :active))
  (is (b/enumerable? {:tag :union :sets '(:active :ready)})))

(deftest unroll-variable-test)

(deftest get-all-elems-from-elem-test
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID1)))
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID2)))
  (is (= '(:PID1 :PID2 :PID3) (b/get-all-elems-from-elem :PID3))))

;; TODO
(deftest pick-bool-var-test)

;; TODO
(deftest get-op-combinations-test)
