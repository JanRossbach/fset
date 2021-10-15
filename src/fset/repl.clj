(ns fset.repl
  (:require
   [fset.util :as util]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.prob.animator :refer [state-space!]]
   [clojure.pprint :as p]
   [lisb.translation.util :refer [b->lisb lisb->b lisb->ir ir->b b->ir ir->ast]]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler (read-string (slurp (str "resources/machines/lisb/source/scheduler.edn"))))

(def scheduler-ir (lisb->ir scheduler))

(def fe-ir (lisb->ir (b->lisb (slurp "resources/machines/b/source/func_extract.mch"))))

(def transform (partial fset/unfset 10 3 true))

(def ss (b/get-statespace (b->ir (slurp "resources/machines/b/source/scheduler.mch"))))

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

  (b/test-fun ss pred-ir)


  (util/rm-typedef-by-id fe-ir :p)

  (b/get-type ss :PID)


  ;; Testing out custom java interop for no particular reason
  (.sayHello (new main.java.Hello "Jan"))

  (p/pprint (ex/extract fe-ir :p))

  (p/pprint scheduler-ir)

  (spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (:ir (transform scheduler-ir :PID))))

  (clojure.pprint/pprint (transform scheduler-ir :PID))

  (clojure.pprint/pprint scheduler-ir)

  (util/get-assigns-by-id scheduler-ir :active)

  (p/pprint (transform scheduler-ir :PID))

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
