(ns fset.repl
  (:require
   [fset.util :as util]
   [clojure.core.match :refer [match]]
   [fset.transformations :as T]
   [fset.core :as fset]
   [fset.varencode :as v]
   [fset.backend :as b]
   [lisb.prob.animator :refer [state-space!]]
   [clojure.pprint :refer [pprint]]
   [lisb.translation.util :refer :all]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.



(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(def train-ss (b/get-statespace train-ir))


(def fe-ir (lisb->ir (b->lisb (slurp "resources/machines/b/source/func_extract.mch"))))

(def scheduler-ir (b->ir (slurp "resources/machines/b/source/scheduler.mch")))

(def scheduler-ss (b/get-statespace scheduler-ir))



(def test-ir (b->ir (slurp "resources/machines/b/source/test.mch")))

(pprint test-ir)

(def invar (util/get-invariant-as-pred scheduler-ir))




(spit "resources/machines/b/source/auto.mch" (ir->b (transform-invar test-ir)))


(def test-expr (lisb->ir '(=> (and (= (intersection :a :b) #{})
                                   (= (union :a :b) #{}))
                              (and (= :a #{}) (= :b #{})))))


(defn preds->b [preds]
  (ir->b {:tag :and :predicates preds}))
