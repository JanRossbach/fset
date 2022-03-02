(ns jan
  (:require
   [hhu.fset.lib.core :as fset]
   [clojure.pprint :refer [pprint]]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [hhu.fset.backend.interface :as b]
   [hhu.fset.encoder.translations :refer [setexpr->bitvector unroll-predicate]]
   [lisb.translation.util :refer [b->ir ir->b]]))

(def jan-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(def mch-dir "components/encoder/resources/encoder/")

(def test-ir (b->ir (slurp (str mch-dir "Test.mch"))))

(spit (str mch-dir "Test-auto.mch") (ir->b (fset/boolencode test-ir :logging true)))

(def machine-pack (str mch-dir "machine-pack/"))

(def can-bus (b->ir (slurp (str machine-pack "CAN_BUS.mch"))))

(def can-bus-encoded (fset/boolencode can-bus :logging true))

(spit (str mch-dir "Core.mch") (ir->b can-bus-encoded))

(def demo-ir (b->ir (slurp (str mch-dir "demo.mch"))))

(pprint can-bus-encoded)

(pprint (setexpr->bitvector {:tag :comprehension-set,
                             :ids [:z],
                             :pred
                             {:tag :and,
                              :preds
                              '({:tag :member, :elem :z, :set :A}
                                {:tag :equals, :left :z, :right :y})}}))

;; Scheduler

(def scheduler-ir (b->ir (slurp "components/encoder/resources/encoder/scheduler.mch"))) ;; Read in the B machine IR from a file

(def test-mch  (b->ir (slurp "components/encoder/resources/encoder/Untitled.mch")))

(def demo-mch-translated (fset/boolencode test-mch :logging true))

(fset/set-config! jan-config)


(defn deff-set-size->num-ops
  [n]
  (let [res (time (fset/boolencode demo-ir
                                   :deff-set-size n
                                   :logging false
                                   :simplify-result false))]
    (b/num-ops res)))

(defn test-deff-set-size [min max]
  (for [i (range min max)]
    (do
      (pprint i)
      (deff-set-size->num-ops i))))


(def results (test-deff-set-size 50 100))

results

(time (b/setup-backend scheduler-ir jan-config))

(pprint (fset/boolencode scheduler-ir))


(spit "components/encoder/resources/encoder/demo-auto.mch" (ir->b demo-mch-translated))

(pprint demo-mch-translated)


(pprint test)

(def scheduler-auto-ir (fset/boolencode scheduler-ir))

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

;(def train-auto-ir (fset/unroll-ops train-ir ))

(def train-auto-ir (fset/boolencode train-ir))

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

(unroll-predicate {:tag :member :elem {:tag :maplet :left :A :right :B} :set {:tag :fn-call :f :nxt :args '(:R1)}})

(setexpr->bitvector {:tag :fn-call :f :nxt :args '(:R1)})

(b/get-elem-index {:tag :maplet :left :A :right :B})

(pprint (simplify-all up))
