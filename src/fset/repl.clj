(ns fset.repl
  (:require
   [clojure.pprint :refer [pprint]]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch"))) ;; Read in the B machine IR from a file

(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(pprint train-ir)


(spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (fset/boolencode scheduler-ir))) ;; Write the translated IR to another file

(def test-ir (b->ir (slurp "resources/test/Test.mch")))

(pprint test-ir)

(b/setup-backend test-ir)

(b/get-all-elems-from-elem :c2)

(b/finite? )

(b/unroll-set-expr :y)
