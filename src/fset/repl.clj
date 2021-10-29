(ns fset.repl
  (:require
   [fset.util :as util]
   [clojure.core.match :refer [match]]
   [fset.transformations :as T]
   [fset.core :as fset]
   [fset.backend :as b]
   [lisb.prob.animator :refer [state-space!]]
   [clojure.pprint :refer [pprint]]
   [lisb.translation.util :refer :all]
   [fset.extract :as ex]))

;; Namespace to run the core functions in a repl and experiment with results.



(def train-ir (b->ir (slurp "resources/machines/b/source/Train_1_beebook_TLC.mch")))

(def train-ss (b/get-statespace train-ir))

(b/get-type train-ss :rtbl)

(pprint (first (util/get-operations train-ir)))

(pprint train-ir)


(def items-ir (b->ir (slurp "resources/machines/b/source/Items.mch")))
(def items-ss (b/get-statespace items-ir))



(def p {:tag :and :predicates '({:tag :member, :element :i, :set :items}
                                {:tag :member :element :j :set :items}
                                {:tag :subset :subset :items
                                 :set {:tag :interval :from 1 :to 10}})})

(def p1 {:tag :equivalence, :predicates '({:tag :intersection, :sets (:a :b)}
                                         {:tag :member, :element :x, :set {:tag :union, :sets (:a :b)}})})

(b/get-possible-var-states items-ss '(:i :j) '(:items) p)

(ir->b (first (T/unroll-predicate p1 '(:x :y :z))))


(lisb->ir '(<=> (intersection :a :b) (member? :x (union :a :b))))


(pprint items-ir)

(defn unroll-op
  [ir]
  (let [op (first (util/get-operations ir))
        {:keys [return parameters body name]} op
        pred (first (:clauses body))]
    pred))

(unroll-op items-ir)



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
