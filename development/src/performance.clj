(ns performance
  (:require
   [criterium.core :refer [bench]]
   [clj-memory-meter.core :as mm]
   [lisb.translation.util :refer [ir->b b->ir]]
   [fset.lib.core :refer [boolencode set-config!]]))

(def performance-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :prob-logging false
   :excluded-vars #{}})

(set-config! performance-config)

(def mch-dir "components/encoder/resources/encoder/")

(def demo1-ir (b->ir (slurp (str mch-dir "Demo(1).mch"))))

(def demo2-ir (b->ir (slurp (str mch-dir "Demo(2).mch"))))

(def demo4-ir (b->ir (slurp (str mch-dir "Demo(4).mch"))))

(defn time-n
  [ir n]
  (boolencode ir
              :deff-set-size n
              :max-unroll-size (inc (* n n))))

(defn experiment
  [machine]
  (for [i (range 2 11)]
    (do
      (println "Deff-set-size: " i)
      (println "---------------")
      (bench (time-n machine i)))))


(comment

  (experiment demo1-ir)

  (experiment demo2-ir)

  (experiment demo4-ir)

)
