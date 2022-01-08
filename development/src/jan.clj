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



(def jan-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :simplify-result true
   :deff-set-size 2
   :logging true
   :excluded-vars #{}})


;; Scheduler

(def scheduler-ir (b->ir (slurp "components/encoder/resources/encoder/scheduler.mch"))) ;; Read in the B machine IR from a file

(def scheduler-auto-ir (fset/boolencode scheduler-ir :logging false :excluded-vars #{:active :ready :waiting} :unroll-sub false))

(pprint (fset/unroll-ops scheduler-ir))


(b/setup-backend scheduler-ir jan-config)

(pprint (fset/boolencode scheduler-ir :excluded-vars #{:active :ready :waiting}))

(pprint scheduler-ir)

(pprint scheduler-auto-ir)

(time (fset/boolencode scheduler-ir))

scheduler-auto-ir

(ir->b scheduler-auto-ir)

(b/model-check (b->ir (ir->b (fset/boolencode scheduler-ir))))

(spit "components/encoder/resources/encoder/scheduler_auto1.mch" (ir->b scheduler-auto-ir))


;; Train


(def train-ir (b->ir (slurp "components/encoder/resources/encoder/Train.mch")))


(pprint train-ir)

(def train-auto-ir (fset/unroll-ops train-ir ))

(def train-auto-ir (fset/boolencode train-ir :logging true :unroll-invariant false))

(pprint train-auto-ir)

(spit "components/encoder/resources/encoder/train_auto1.mch" (ir->b train-auto-ir)) ;; Write the translated IR to another file

(b/setup-backend train-ir jan-config)

(def up (unroll-predicate {:tag :for-all,
                           :ids [:x :y],
                           :implication
                           {:tag :implication,
                            :preds
                            '({:tag :and,
                               :preds
                               ({:tag :member, :elem :x, :set :BLOCKS}
                                {:tag :member, :elem :y, :set :BLOCKS}
                                {:tag :member,
                                 :elem {:tag :maplet, :left :x, :right :y},
                                 :set :TRK})}
                              {:tag :exists,
                               :ids [:r],
                               :pred
                               {:tag :and,
                                :preds
                                ({:tag :member, :elem :r, :set :ROUTES}
                                 {:tag :member,
                                  :elem {:tag :maplet, :left :x, :right :y},
                                  :set {:tag :fn-call, :f :nxt, :args (:r)}})}})}}))
