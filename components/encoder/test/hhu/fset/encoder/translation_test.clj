(ns hhu.fset.encoder.translation-test
  (:require
   [clojure.test :refer [deftest testing is are]]
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

(def test-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(deftest unroll-predicate-test
  (b/setup-backend scheduler test-config)
  (are [x y] (= x (ir->b (simplify-all (trans/unroll-predicate (b->ir y)))))
    "not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE) & not(activePID3=TRUE or readyPID3=TRUE)"
    "#PREDICATEactive\\/ready={}"

    "not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE) & not(activePID3=TRUE or readyPID3=TRUE)"
    "#PREDICATE{}=active\\/ready"

    "not(not(activePID1=TRUE or readyPID1=TRUE) & not(activePID2=TRUE or readyPID2=TRUE) & not(activePID3=TRUE or readyPID3=TRUE))"
    "#PREDICATE{}/=active\\/ready"

    "TRUE=TRUE"
    "#PREDICATEactive<:PID"

    "(activePID1=TRUE => readyPID1=TRUE) & (activePID2=TRUE => readyPID2=TRUE) & (activePID3=TRUE => readyPID3=TRUE)"
    "#PREDICATEactive<:ready"

    "activePID1=TRUE & not(readyPID1=TRUE) or (activePID2=TRUE & not(readyPID2=TRUE)) or (activePID3=TRUE & not(readyPID3=TRUE)) & (activePID1=TRUE => readyPID1=TRUE) & (activePID2=TRUE => readyPID2=TRUE) & (activePID3=TRUE => readyPID3=TRUE)"
    "#PREDICATEactive<<:ready"

    "activePID1=TRUE <=> readyPID1=TRUE & activePID2=TRUE <=> readyPID2=TRUE & activePID3=TRUE <=> readyPID3=TRUE"
    "#PREDICATEactive=ready"

    "activePID1=TRUE & activePID2=TRUE & activePID3=TRUE"
    "#PREDICATE!(x).(x:PID => x:active)"

    "activePID1=TRUE or activePID2=TRUE or activePID3=TRUE"
    "#PREDICATE#(x).(x:PID & x:active)"

    "activePID1=TRUE"
    "#PREDICATEPID1:active"

    "1+1=3-2"
    "#PREDICATE 1+1=3-2"))
