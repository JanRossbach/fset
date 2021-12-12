(ns fset.repl
  (:require
   [clojure.pprint :refer [pprint]]
   [fset.dsl :as dsl]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch"))) ;; Read in the B machine IR from a file

(def numbers-ir (b->ir (slurp "resources/test/Numbers.mch")))

(b/setup-backend scheduler-ir)

(pprint (fset/boolencode scheduler-ir))

(ir->b (fset/boolencode scheduler-ir))

(spit "resources/machines/b/target/scheduler_auto1.mch" (ir->b (fset/boolencode scheduler-ir))) ;; Write the translated IR to another file

(spit "resources/test/scheduler-ir.edn" (fset/boolencode scheduler-ir))

(def test-ir (b->ir (slurp "resources/machines/b/source/test.mch")))

(pprint test-ir)

(b/setup-backend test-ir)

(pprint (fset/boolencode test-ir))

(spit "resources/machines/b/target/test_auto.mch" (ir->b (fset/boolencode test-ir)))

(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(b/setup-backend train-ir)

(def train-ir-auto (fset/boolencode train-ir))

(spit "resources/machines/b/target/train_auto.mch" (ir->b (fset/boolencode train-ir)))

(pprint (fset/boolencode scheduler-ir))


(ir->b (fset/boolencode scheduler-ir))

(b/eval-constant :nxt)

(pprint (fset/boolencode train-ir))

(fset/boolencode train-ir)

(pprint (b/eval-constant :nxt))

(ir->b train-ir-auto)

(defn boolcount [var]
  (count (filter (fn [b] (= (:var b) var)) (b/get-all-bools))))

(b/setup-backend train-ir)
