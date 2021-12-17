(ns fset.repl
  (:require
   [clojure.pprint :refer [pprint]]
   [fset.dsl :as dsl]
   [fset.expressions :refer [unroll-expression]]
   [fset.predicates :refer [unroll-predicate]]
   [fset.simplify :refer [simplify-all]]
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

(ir->b train-ir-auto)

train-ir-auto


(b/unrollable-var? :TRK)

(b/unroll-variable :TRK)


(pprint train-ir-auto)

(pprint (fset.simplify/simplify-all (unroll-predicate {:tag :member :elem {:tag :fn-call :f :fst :args '(:R9)} :set {:tag :difference
                                                                                                                     :sets '(:resbl :OCC)}})))
(spit "resources/machines/b/target/train_auto1.mch" (ir->b train-ir-auto))

(unroll-predicate {:tag :and, :preds '({:tag :member, :elem :R10, :set :frm} {:tag :member, :elem {:tag :fn-call, :f :fst, :args (:R10)}, :set {:tag :difference, :sets (:resbl :OCC)}}
                                       {:tag :equals, :left {:tag :fn-call, :f :rsrtbl, :args ({:tag :fn-call, :f :fst, :args (:R10)})}, :right :R10})})

(unroll-predicate {:tag :member :elem :R10 :set :frm})

(pprint (fset.simplify/simplify-all (unroll-predicate {:tag :equals, :left {:tag :fn-call, :f :rsrtbl, :args '({:tag :fn-call, :f :fst, :args (:R10)})}, :right :R10})))



(pprint (fset.simplify/simplify-all (unroll-predicate {:tag :domain-restriction
                                                       :set
                                                       {:tag :image
                                                        :rel {:tag :inverse :rel :rsrtbl}
                                                        :set #{:R1}}
                                                       :rel :TRK})))

(pprint train-ir)


(unroll-expression {:tag :union :sets (list :LBT #{{:tag :fn-call :f :fst :args (list :R1)}})})

(unroll-expression {:tag :fn-call, :f :rsrtbl, :args '({:tag :fn-call, :f :fst, :args (:R10)})})

(b/get-type :LBT)


(pprint (fset.simplify/simplify-all (unroll-expression {:tag :fn-call, :f :fst, :args (list :R8)})))

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


(unroll-predicate {:tag :member :elem {:tag :fn-call :f :fst :args '(:R1)} :set {:tag :difference :sets '(:resbl :OCC)}})

(pprint train-ir-auto)

(b/setup-backend train-ir)

(b/unrollable-var? :TRK)

(b/unroll-variable :frm)

(b/eval-constant :nxt)

(ir->b train-ir-auto)

(pprint train-ir-auto)

(pprint (simplify-all (fset/unroll-sub {:tag :assignment :id-vals '(:OCC {:tag :union :sets (:OCC #{{:tag :fn-call :f :fst :args (:R1)}})})})))

(count (simplify-all (unroll-expression #{{:tag :fn-call :f :fst :args '(:R1)}})))






(def uexpr (unroll-expression {:tag :fn-call :f :fst :args '(:R1)}))



(def enumeration-set #{{:tag :fn-call :f :fst :args '(:R1)}})
