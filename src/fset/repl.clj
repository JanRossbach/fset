(ns fset.repl
  (:require
   [clojure.pprint :refer [pprint]]
   [fset.dsl :as dsl]
   [fset.expressions :refer [unroll-expression]]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))

;; Namespace to run the core functions in a repl and experiment with results.

;; SCHEDULER

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch"))) ;; Read in the B machine IR from a file

(def scheduler-auto-ir (fset/boolencode scheduler-ir))

(def numbers-ir (b->ir (slurp "resources/test/Numbers.mch")))

(b/model-check (b->ir (ir->b (fset/boolencode scheduler-ir))))



(b/setup-backend scheduler-ir)

(unroll-expression :active)

(pprint (fset/boolencode scheduler-ir))

(ir->b (fset/boolencode scheduler-ir))

(spit "resources/machines/b/target/scheduler_auto1.mch" (ir->b (fset/boolencode scheduler-ir))) ;; Write the translated IR to another file

(spit "resources/test/scheduler-ir.edn" (fset/boolencode scheduler-ir))


;; TRAIN

(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(def train-ir-auto (fset/boolencode train-ir))

(ir->b (fset/boolencode train-ir))

(spit "resources/machines/b/target/train_auto1.mch" (ir->b train-ir-auto))

(pprint train-ir-auto)

(defn boolcount [var]
  (count (filter (fn [b] (= (:var b) var)) (b/get-all-bools))))

(b/setup-backend train-ir)

(b/unroll-variable :frm)

(b/eval-constant :nxt)

(ir->b train-ir-auto)

(pprint train-ir-auto)

(b/setup-backend scheduler-ir)
