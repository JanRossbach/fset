(ns fset.experiment
  (:require
   [fset.util :as util]
   [fset.core :as fset]
   [fset.backend.variables :refer [generate-variables]]
   [clojure.pprint :as p]
   [lisb.translation.util :refer [b->lisb lisb->b lisb->ir ir->b b->ir]]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler (read-string (slurp (str "resources/machines/lisb/source/scheduler.edn"))))

(def scheduler-ir (lisb->ir scheduler))

(def fe-ir (lisb->ir (b->lisb (slurp "resources/machines/b/source/func_extract.mch"))))

(def transform (partial fset/transform 10 3 true))

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

  (clojure.pprint/pprint (generate-variables {:variables {} :ir scheduler-ir :target-set :PID :set-size 3}))

  (util/resolve-type {:variables {} :ir scheduler-ir :set-to-rewrite :PID :deferred-set-size 3} :active {:tag :power-set :set :PID})

  (util/get-assigns-by-id fe-ir :p)

  (util/rm-typedef-by-id fe-ir :p)

  ;; Testing out custom java interop for no particular reason
  (.sayHello (new main.java.Hello "Jan"))

  (p/pprint (ex/extract fe-ir :p))

  (p/pprint scheduler-ir)

  (spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (:ir (transform scheduler-ir :PID))))

  (clojure.pprint/pprint (transform scheduler-ir :PID)))

  (util/get-assigns-by-id scheduler-ir :active)

  (p/pprint (fset/transform scheduler-ir :active))

  (ir->b {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses '({:tag :init
                                                                                                                                     :substitution {:tag :parallel-substitution
                                                                                                                                                    :substitutions ()}})}))
