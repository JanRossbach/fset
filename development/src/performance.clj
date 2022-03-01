(ns performance
  (:require
   [criterium.core :refer [bench]]
   [lisb.translation.util :refer [ir->b b->ir]]
   [hhu.fset.lib.core :refer [boolencode set-config!]]))

(def performance-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(set-config! performance-config)

(def mch-dir "components/encoder/resources/encoder/")

(def train-ir (b->ir (slurp (str mch-dir "Train.mch"))))

(def train-encoded (boolencode train-ir))

(def scheduler-ir (b->ir (slurp (str mch-dir "scheduler.mch"))))

(def demo-ir (b->ir (slurp (str mch-dir "demo.mch"))))

(defn time-n
  [ir n]
  (boolencode ir
              :deff-set-size n
              :max-unroll-size (inc (* n n))))

(defn experiment
  [machine min max step]
  (for [i (range min max step)]
    (do
      (println "Deff-set-size: " i)
      (println "---------------")
      (bench (time-n machine i)))))


(comment

  (spit (str mch-dir "Train-auto.mch") (ir->b train-encoded))

  (bench (boolencode train-ir))

  (bench (ir->b train-encoded))

  (bench (ir->b (boolencode train-ir)))

  (time (boolencode train-ir))

  (time (ir->b train-ir))

  (ir->b scheduler-ir)

  (time (ir->b train-encoded))

  (time (ir->b (boolencode train-ir)))

  (experiment demo-ir 500000 1000000 100000)

  (experiment scheduler-ir 500000 1000000 100000)

  (bench (time-n demo-ir 1000000))

  (time-n demo-ir 1000000))
