(ns fset.repl
  (:require
   [clojure.pprint :refer [pprint]]
   [fset.core :as fset]
   [lisb.translation.util :refer [b->ir ir->b]]))

;; Namespace to run the core functions in a repl and experiment with results.

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch"))) ;; Read in the B machine IR from a file

(spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (fset/boolencode scheduler-ir))) ;; Write the translated IR to another file

(def numbers-ir (b->ir (slurp "resources/test/Numbers.mch")))

;;(pprint (fset/boolencode numbers-ir))
