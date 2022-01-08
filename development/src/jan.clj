(ns dev.jan
  (:require
   [hhu.fset.lib.core :as fset]
   [taoensso.timbre :as log]
   [clojure.pprint :refer [pprint]]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [hhu.fset.backend.interface :as b]
   [hhu.fset.backend.core :refer [eval-constant]]
   [hhu.fset.backend.lisb-util :refer [get-statespace]]
   [lisb.translation.lisb2ir :refer [bmaplet]]
   [hhu.fset.encoder.expressions :refer [setexpr->bitvector]]
   [hhu.fset.encoder.predicates :refer [unroll-predicate]]
   [lisb.translation.util :refer [b->ir ir->b lisb->ir]]))

(setexpr->bitvector {:tag :union, :sets '(:rsrtbl {:tag :range-restriction, :rel :rtbl, :set #{:R9}})})

(unroll-predicate {:tag :and, :preds '({:tag :member, :elem :R9, :set :frm} {:tag :member, :elem {:tag :fn-call, :f :fst, :args (:R9)}, :set {:tag :difference, :sets (:resbl :OCC)}} {:tag :equals, :left {:tag :fn-call, :f :rsrtbl, :args ({:tag :fn-call, :f :fst, :args (:R9)})}, :right :R9})})

(unroll-predicate {:tag :equals, :left {:tag :fn-call, :f :rsrtbl, :args '({:tag :fn-call, :f :fst, :args (:R9)})}, :right :R9})



(b/get-type-elem-matrix {:tag :maplet :left :A :right :B})

(b/setup-backend train-ir jan-config)

(def jan-config
  {:max-unroll-size 200
   :unroll-invariant true
   :simplify-result true
   :deff-set-size 2
   :logging true
   :excluded-vars #{}})


  (fset/reset-config)

  (def scheduler-ir (b->ir (slurp "components/encoder/resources/encoder/scheduler.mch"))) ;; Read in the B machine IR from a file

  (def scheduler-auto-ir (fset/boolencode scheduler-ir :logging true))

  (pprint scheduler-ir)

  (pprint scheduler-auto-ir)

  (time (fset/boolencode scheduler-ir))

  scheduler-auto-ir

  (ir->b scheduler-auto-ir)

  (b/model-check (b->ir (ir->b (fset/boolencode scheduler-ir))))

  (spit "resources/test/scheduler-ir.edn" (fset/boolencode scheduler-ir))

  (def train-ir (b->ir (slurp "components/encoder/resources/encoder/Train.mch")))

  (def train-auto-ir (fset/boolencode train-ir :excluded-vars #{} :logging true))

  (spit "components/encoder/resources/encoder/train_auto1.mch" (ir->b train-auto-ir)) ;; Write the translated IR to another file

  (spit "components/encoder/resources/encoder/scheduler_auto1.mch" (ir->b scheduler-auto-ir))
