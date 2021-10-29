(ns fset.backend-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [b->ir]]
   [fset.backend :as b]))

(def scheduler-ir (b->ir (slurp "resources/test/scheduler.mch")))

(deftest loading-statespace-test
  (testing "The returned statespace has the correct Type."
      (is (= de.prob.statespace.StateSpace (type (b/get-statespace scheduler-ir))))))


(def scheduler-ss (b/get-statespace scheduler-ir))


(deftest set-elems-test
  (testing "The correct elements are returned."
   (is (= #{"PID1" "PID2" "PID3"} (b/set-elems scheduler-ss :PID)))))

(deftest type-test
  (testing "Powerset Type"
    (is (= "POW(PID)" (b/get-type scheduler-ss :active)))
    (is (= "POW(PID)" (b/get-type scheduler-ss :ready)))
    (is (= "POW(PID)" (b/get-type scheduler-ss :waiting)))))
