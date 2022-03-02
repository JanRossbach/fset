(ns hhu.fset.encoder.core-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [lisb.translation.util :refer [b->ir ir->b]]
   [hhu.fset.encoder.core :as core]
   [hhu.fset.backend.interface :as b]))

(def mch-dir "components/encoder/resources/encoder/test/")

(def scheduler (b->ir (slurp (str mch-dir "scheduler.mch"))))

(def test-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(deftest machine-clauses-test
  (testing "Scheduler"
    (b/setup-backend scheduler test-config)
    (are [x y] (= x (ir->b (core/unroll-clause (b->ir (str "#MACHINECLAUSE" y)))))
      "VARIABLES activePID1, activePID2\n"
      "VARIABLES active"

      "INVARIANT activePID1:BOOL & activePID2:BOOL & readyPID1:BOOL & readyPID2:BOOL & waitingPID1:BOOL & waitingPID2:BOOL & TRUE=TRUE & (not(activePID1=TRUE) & not(activePID2=TRUE))\n"
      "INVARIANT active:POW(PID) & active={}"

      "INITIALISATION activePID1 := FALSE || activePID2 := FALSE\n"
      "INITIALISATION active:={}"

      "ASSERTIONS\nnot(activePID1=TRUE) & not(activePID2=TRUE)\n"
      "ASSERTIONS active={}"

      "OPERATIONS\nnewPID1 = SELECT not(activePID1=TRUE) & not(readyPID1=TRUE or waitingPID1=TRUE) THEN waitingPID1 := TRUE END ;\nnewPID2 = SELECT not(activePID2=TRUE) & not(readyPID2=TRUE or waitingPID2=TRUE) THEN waitingPID2 := TRUE END \n"
      "OPERATIONS new(pp) =
    SELECT
        pp : PID  &
        pp /: active &
        pp /: (ready \\/ waiting)
    THEN
        waiting := (waiting \\/ { pp })
    END")))
