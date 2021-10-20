(ns fset.repl
  (:require
   [fset.util :as util]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.prob.animator :refer [state-space!]]
   [clojure.pprint :refer [pprint]]
   [lisb.translation.util :refer [b->lisb lisb->b lisb->ir ir->b b->ir ir->ast]]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.





(def fe-ir (lisb->ir (b->lisb (slurp "resources/machines/b/source/func_extract.mch"))))

(def transform (partial fset/boolencode 10 3 true))

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch")))

(def test-ir (b->ir (slurp "resources/machines/b/source/test.mch")))

(def test-ss (b/get-statespace test-ir))

(def scheduler-ss (b/get-statespace scheduler-ir))

(def api-result (b/get-possible-var-states test-ss '(:active :ready :waiting) (util/get-invariant-as-pred scheduler-ir)))

(b/get-possible-var-states test-ss '(:ready) '(:active :waiting) (util/get-invariant-as-pred scheduler-ir))

(defn interpret-api-result
  [vars result]
  )

(interpret-api-result '(:active :ready :waiting))


(util/get-invariant-as-pred scheduler-ir)





(def mutex-ir (b->ir (slurp "resources/machines/b/source/mutex.mch")))
(def mutex-ss (b/get-statespace mutex-ir))

(spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (:ir (fset/boolencode 10 3 true scheduler-ir :PID))))

(def lift-ir (b->ir (slurp "resources/machines/b/source/Lift.mch")))

(def lift-ss (state-space! (ir->ast lift-ir)))


(def lift-vars (util/get-vars lift-ir))

(def lift-invar (:predicate (util/get-invariant lift-ir)))

(b/get-possible-var-states lift-ss lift-vars lift-invar)


(b/get-possible-var-states scheduler-ss [:active :ready :waiting] (:predicate (util/get-invariant scheduler-ir)))


(defn test-fun
  []
  (let [fn "resources/machines/b/source/test.mch"
        ir (b->ir (slurp fn))
        ss (b/get-statespace ir)
        vars (util/get-vars ir)
        invar (util/get-invariant ir)]
    (b/get-possible-var-states ss vars (:predicate invar))))

(test-fun)

(p/pprint (b->ir (slurp "resources/machines/b/source/test.mch")))

(ir->b (b->ir (slurp "resources/machines/b/source/test.mch")))

;; repl with example of executing the high level commands
(comment

  (clojure.pprint/pprint scheduler-ir)

  (ir->b (util/clear-sets scheduler-ir))

  (util/clear-sets {:clauses '({:tag :sets
                                :identifiers (:PID)}
                               {:tag variables
                                :identifiers (:hello)})})

  (spit "resources/machines/b/target/func_extract.mch" (ir->b (ex/extract fe-ir :p)))

  (p/pprint (ex/extract fe-ir :p))

  (clojure.pprint/pprint (fset/generate-variables {:variables {} :ir scheduler-ir :target-set :PID :set-size 3}))

  (util/resolve-type {:variables {} :ir scheduler-ir :set-to-rewrite :PID :deferred-set-size 3} :active {:tag :power-set :set :PID})

  (util/get-assigns-by-id fe-ir :p)

  (fset/unfset-operations scheduler-ir)

  (clojure.pprint/pprint scheduler-ir)

;; TODO SELBES PROBLEM? Idnetifier not initialized
  (b/get-possible-var-states ss :pp
                             {:tag :member :element :pp :set :waiting})

  (util/rm-typedef-by-id fe-ir :p)

  (util/get-sets scheduler-ir)

  (util/get-set-ids scheduler-ir)

  (fset/determine-target-sets scheduler-ir 10 3)

  (b/get-type ss :PID)

  (p/pprint (ex/extract fe-ir :p))

  (pprint (fset/boolencode 10 3 true scheduler-ir :PID))

  (spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (:ir (fset/boolencode 10 3 true scheduler-ir :PID))))

  (pprint (transform scheduler-ir :PID))

  (clojure.pprint/pprint scheduler-ir)

  (util/get-assigns-by-id scheduler-ir :active)

  (ir->b (transform scheduler-ir :PID))

  (ir->b {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses '({:tag :init
                                                                                                                                     :substitution {:tag :parallel-substitution
                                                                                                                                                    :substitutions ()}})})

  (def test-lisb
    '(for [v '(:PID1 :PID2 :PID3)]
       (bor v :active)))

  (def vars {:active [:activePID1 :activePID2 :activePID3] :ready [:readyPID1 :readyPID2 :readyPID3] :waiting [:waitingPID1 :waitingPID2 :waitingPID3]})

  (defn ors [_] [[:activePID1 :readyPID1] [:activePID2 :readyPID2] [:activePID3 :readyPID3]])

  (ir->b {:tag :and
          :predicates (lisb->ir '(for [[l r] (ors vars)] (= (bpred->bool (bor (= l true) (= r true))) false)))}))
