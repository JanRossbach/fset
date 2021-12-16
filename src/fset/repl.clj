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

(pprint scheduler-auto-ir)

(ir->b scheduler-auto-ir)

(def numbers-ir (b->ir (slurp "resources/test/Numbers.mch")))

(b/model-check (b->ir (ir->b (fset/boolencode scheduler-ir))))

(b/setup-backend scheduler-ir)

(b/get-elem-index :PID3)

(unroll-expression :active)

(pprint (fset/boolencode scheduler-ir))

(ir->b (fset/boolencode scheduler-ir))

(spit "resources/machines/b/target/scheduler_auto1.mch" (ir->b (fset/boolencode scheduler-ir))) ;; Write the translated IR to another file

(spit "resources/test/scheduler-ir.edn" (fset/boolencode scheduler-ir))


;; TRAIN

(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(def train-ir-auto (fset/boolencode train-ir))

(b/setup-backend train-ir)

(def c (set (first (b/eval-constant :fst))))



(pprint (map (fn [elem] (if (contains? c elem) :true :false)) (b/get-sub-type-elems :fst)))



(b/get-type :rsrtbl)


(pprint (unroll-expression :rsrtbl))










()











train-ir-auto

(pprint "---------------------------")

(pprint train-ir-auto)

(ir->b train-ir-auto)

(ir->b (fset/boolencode train-ir))

(spit "resources/machines/b/target/train_auto1.mch" (ir->b train-ir-auto))

(pprint train-ir-auto)

(b/setup-backend train-ir)

(b/unrollable-var? :TRK)

(b/unroll-variable :frm)

(b/eval-constant :nxt)

(ir->b train-ir-auto)

(pprint train-ir-auto)
