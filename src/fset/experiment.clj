(ns fset.experiment
  (:require
   [fset.config :as cfg]
   [fset.util :as util]
   [fset.core :as fset]
   [clojure.pprint :as p]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.translation.util :refer [b->lisb lisb->b lisb->ir ir->b b->ir]]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler (read-string (slurp (str cfg/lisb-source-dir "scheduler.edn"))))

(def scheduler-ir (lisb->ir scheduler))

(def scheduler-mch
  (fset/make-mch! (read-string (slurp "resources/machines/lisb/source/scheduler.edn")) cfg/meta-data))

(def fe-ir (lisb->ir (b->lisb (slurp "resources/machines/b/source/func_extract.mch"))))

(defn save-b!
  [fn content]
  (spit (str cfg/b-target-dir cfg/prefix fn) content))

;; repl with example of executing the high level commands
(comment
  (save-b! "scheduler.mch" scheduler-ir)

  (clojure.pprint/pprint scheduler-ir)

  (ir->b (util/clear-sets scheduler-ir))

  (util/clear-sets {:clauses '({:tag :sets
                                :identifiers (:PID)}
                               {:tag variables
                                :identifiers (:hello)})})

  (eval-ir-formula (:ss scheduler-mch) (lisb->ir '(comp-set [:x] (member? :x :PID))))

  (clojure.pprint/pprint (:ir scheduler-mch))

  (spit "resources/machines/b/target/func_extract.mch" (ir->b (ex/extract fe-ir :p)))

  (p/pprint (ex/extract fe-ir :p))

  (util/get-assigns-by-id fe-ir :p)

  (util/rm-typedef-by-id fe-ir :p)

  (p/pprint (ex/extract fe-ir :p))


  (save-b! "func_extract.mch" (ir->b (ex/extract fe-ir :p)))

  (ir->b {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses '({:tag :init
                                                                                                                                     :substitution {:tag :parallel-substitution
                                                                                                                                                    :substitutions ()}})})
  )
