(ns fset.repl
  (:require
   [fset.util :as util]
   [clojure.core.match :refer [match]]
   [fset.core :as fset]
   [fset.extract :as e]
   [fset.dsl :refer [AND]]
   [fset.backend :as b]
   [lisb.prob.animator :refer [state-space!]]
   [clojure.pprint :refer [pprint]]
   [lisb.translation.util :refer :all]))

;; Namespace to run the core functions in a repl and experiment with results.



(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(def train-ss (b/get-statespace train-ir))

(b/get-type train-ss :rtbl)

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch")))

(def scheduler-ss (b/get-statespace scheduler-ir))

(b/get-possible-var-states scheduler-ss [:ready] [:active :waiting] (util/get-invariant-as-pred scheduler-ir))



(def test-expr (lisb->ir '(=> (and (= (intersection :a :b) #{})
                                   (= (union :a :b) #{}))
                              (and (= :a #{}) (= :b #{})))))
(pprint scheduler-ir)

(ir->b (fset/boolencode scheduler-ir))

(pprint (fset/boolencode scheduler-ir))

(spit "resources/machines/b/target/scheduler_auto.mch" (ir->b (fset/boolencode scheduler-ir)))

(def extract-ir (b->ir (slurp "resources/machines/b/source/func_extract.mch")))

(pprint (e/extract extract-ir :p))


(pprint extract-ir)
