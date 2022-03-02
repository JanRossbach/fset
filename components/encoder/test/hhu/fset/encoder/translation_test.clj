(ns hhu.fset.encoder.translation-test
  (:require
   [clojure.test :refer [deftest testing are]]
   [clojure.core.matrix :as m]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [lisb.translation.util :refer [b->ir ir->b]]
   [hhu.fset.dsl.interface :refer [TRUE FALSE]]
   [hhu.fset.encoder.translations :as trans]
   [hhu.fset.backend.interface :as b]))

(def A (m/matrix [[TRUE FALSE] [FALSE TRUE]]))
(def B (m/matrix [[FALSE TRUE] [TRUE FALSE]]))
(def C (m/matrix [[TRUE TRUE] [TRUE TRUE]]))
(def D (m/matrix [[FALSE FALSE] [FALSE FALSE]]))

(deftest matrix-operations-test
  (are [x y] (= x y)
    (m/transpose A) (trans/bmtranspose A)
    C (simplify-all (trans/bmadd A B))
    D (simplify-all (trans/bemul A B))
    A (simplify-all (trans/bmmul A A))
    C (trans/bemap (constantly TRUE) A)))


(def mch-dir "components/encoder/resources/encoder/test/")
(def scheduler (b->ir (slurp (str mch-dir "scheduler.mch"))))
(def demo (b->ir (slurp (str mch-dir "demo.mch"))))

(def test-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})



(deftest setexpr->bitvector-test
  (testing "Sets via Scheduler"
    (b/setup-backend scheduler test-config)
    (are [x y] (= x (mapv (comp ir->b simplify-all) (trans/setexpr->bitvector (b->ir (str "#EXPRESSION" y)))))
      ["TRUE=TRUE" "TRUE=TRUE"]
      "PID"

      ["activePID1=TRUE" "activePID2=TRUE"]
      "active"

      ["activePID1=TRUE" "activePID2=TRUE"]
      "{x|x:PID & x:active}"

      ["TRUE=TRUE" "TRUE=FALSE"]
      "{PID1}"

      ["TRUE=FALSE" "TRUE=TRUE" "TRUE=TRUE" "TRUE=FALSE"]
      "{PID1|->PID2,PID2|->PID1}"

      ["activePID1=TRUE or readyPID1=TRUE"
       "activePID2=TRUE or readyPID2=TRUE"]
      "active\\/ready"

      ["activePID1=TRUE & readyPID1=TRUE" "activePID2=TRUE & readyPID2=TRUE"]
      "active/\\ready"

      ["activePID1=TRUE & not(readyPID1=TRUE)"
       "activePID2=TRUE & not(readyPID2=TRUE)"]
      "active-ready"

      ["readyPID1=TRUE or waitingPID1=TRUE or activePID1=TRUE"
       "readyPID2=TRUE or waitingPID2=TRUE or activePID2=TRUE"]
      "union({active,ready,waiting})"

      ["readyPID1=TRUE & waitingPID1=TRUE & activePID1=TRUE"
       "readyPID2=TRUE & waitingPID2=TRUE & activePID2=TRUE"]
      "inter({active,ready,waiting})"))
  (testing "Relations via Demo"
    (b/setup-backend demo test-config)
    (are [x y] (= x (mapv (comp ir->b simplify-all) (trans/setexpr->bitvector (b->ir (str "#EXPRESSION" y)))))
      ["TRUE=TRUE" "TRUE=TRUE" "TRUE=TRUE" "TRUE=TRUE"]
      "S*S"

      ["rS1S1=TRUE" "rS1S2=TRUE" "rS2S1=TRUE" "rS2S2=TRUE"]
      "id(r)"

      ["rS1S1=TRUE" "rS2S1=TRUE" "rS1S2=TRUE" "rS2S2=TRUE"]
      "r~"

      ["rS1S1=TRUE" "rS1S2=TRUE"]
      "r[S1]"

      ["rS1S1=TRUE" "rS1S2=TRUE"]
      "r(S1)"

      ["rS1S1=TRUE or rS1S2=TRUE" "rS2S1=TRUE or rS2S2=TRUE"]
      "dom(r)"

      ["rS1S1=TRUE or rS2S1=TRUE" "rS1S2=TRUE or rS2S2=TRUE"]
      "ran(r)"

      ["rS1S1=TRUE" "rS1S2=TRUE" "TRUE=FALSE" "TRUE=FALSE"]
      "{S1}<|r"

      ["TRUE=FALSE" "TRUE=FALSE" "rS2S1=TRUE" "rS2S2=TRUE"]
      "{S1}<<|r"

      ["rS1S1=TRUE" "TRUE=FALSE" "rS2S1=TRUE" "TRUE=FALSE"]
      "r|>{S1}"

      ["TRUE=FALSE" "rS1S2=TRUE" "TRUE=FALSE" "rS2S2=TRUE"]
      "r|>>{S1}"

      ["rS1S1=TRUE & rS1S1=TRUE or (rS1S2=TRUE & rS2S1=TRUE)"
       "rS1S1=TRUE & rS1S2=TRUE or (rS1S2=TRUE & rS2S2=TRUE)"
       "rS2S1=TRUE & rS1S1=TRUE or (rS2S2=TRUE & rS2S1=TRUE)"
       "rS2S1=TRUE & rS1S2=TRUE or (rS2S2=TRUE & rS2S2=TRUE)"]
      "(r;r)"

      ["rS1S1=TRUE or rS1S2=TRUE or (rS1S1=TRUE or rS2S1=TRUE)"
       "rS2S1=TRUE or rS2S2=TRUE or (rS1S2=TRUE or rS2S2=TRUE)"]
      "iterate(r,0)"

      ["rS1S1=TRUE & (rS1S1=TRUE or rS1S2=TRUE or (rS1S1=TRUE or rS2S1=TRUE))"
       "rS1S1=TRUE & (rS2S1=TRUE or rS2S2=TRUE or (rS1S2=TRUE or rS2S2=TRUE))"
       "rS2S1=TRUE & (rS1S1=TRUE or rS1S2=TRUE or (rS1S1=TRUE or rS2S1=TRUE))"
       "rS2S1=TRUE & (rS2S1=TRUE or rS2S2=TRUE or (rS1S2=TRUE or rS2S2=TRUE))"]
      "iterate(r,1)"

      ["TRUE=FALSE" "TRUE=TRUE" "rS2S1=TRUE" "rS2S2=TRUE"]
      "r<+{S1|->S2}")))


(deftest unroll-predicate-test
  (testing "Scheduler"
    (b/setup-backend scheduler test-config)
    (are [x y] (= x (ir->b (simplify-all (trans/unroll-predicate (b->ir (str "#PREDICATE" y))))))
      "not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE)"
      "active\\/ready={}"

      "not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE)"
      "{}=active\\/ready"

      "not(not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE))"
      "{}/=active\\/ready"

      "TRUE=TRUE"
      "active<:PID"

      "(activePID1=TRUE => readyPID1=TRUE) & (activePID2=TRUE => readyPID2=TRUE)"
      "active<:ready"

      "activePID1=TRUE & not(readyPID1=TRUE) or (activePID2=TRUE & not(readyPID2=TRUE)) & (activePID1=TRUE => readyPID1=TRUE) & (activePID2=TRUE => readyPID2=TRUE)"
      "active<<:ready"

      "activePID1=TRUE <=> readyPID1=TRUE & activePID2=TRUE <=> readyPID2=TRUE"
      "active=ready"

      "activePID1=TRUE & activePID2=TRUE"
      "!(x).(x:PID => x:active)"

      "activePID1=TRUE or activePID2=TRUE"
      "#(x).(x:PID & x:active)"

      "activePID1=TRUE"
      "PID1:active"

      "1+1=3-2"
      "1+1=3-2")))


(deftest substitutions-test
  (testing "Scheduler"
    (b/setup-backend scheduler test-config)
    (are [x y] (= x (ir->b (simplify-all (trans/unroll-sub (b->ir (str "#SUBSTITUTION" y))))))
      "activePID1 := FALSE || activePID2 := FALSE"
      "active:={}"

      "activePID1 := TRUE || activePID2 := TRUE"
      "active:=PID"

      "activePID1 :(not(activePID1=TRUE) & not(activePID2=TRUE))  || activePID2 :(not(activePID1=TRUE) & not(activePID2=TRUE)) "
      "active:(active={})"

      "activePID1 := FALSE || activePID2 := FALSE ; readyPID1 := FALSE || readyPID2 := FALSE"
      "active:={};ready:={}"

      "activePID1 := TRUE || activePID2 := FALSE"
      "LET x BE x=PID1 IN active:={x} END"

      "PRE not(activePID1=TRUE) & not(activePID2=TRUE) THEN activePID1 := TRUE || activePID2 := TRUE END "
      "PRE active={} THEN active:=PID END"

      "ASSERT not(activePID1=TRUE) & not(activePID2=TRUE) THEN activePID1 := TRUE || activePID2 := TRUE END "
      "ASSERT active={} THEN active:=PID END"

      "IF not(activePID1=TRUE) & not(activePID2=TRUE) THEN activePID1 := TRUE || activePID2 := TRUE ELSE skip END "
      "IF active={} THEN active:=PID ELSE skip END"

      "CHOICE activePID1 := FALSE || activePID2 := FALSE OR activePID1 := TRUE || activePID2 := TRUE END "
      "CHOICE active:={} OR active:=PID END"

      "activePID1 := FALSE || activePID2 := FALSE || readyPID1 := FALSE || readyPID2 := FALSE"
      "active:={}||ready:={}"

      "SELECT not(activePID1=TRUE) & not(activePID2=TRUE) THEN activePID1 := TRUE || activePID2 := FALSE END "
      "SELECT active={} THEN active:={PID1} END"

      "activePID1 := bool(PID1:x) || activePID2 := bool(PID2:x)"
      "ANY x WHERE x:PID THEN active:=x END")))
