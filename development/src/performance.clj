(ns performance
  (:require
   [hhu.fset.lib.core :refer [boolencode set-config!]]
   [lisb.translation.util :refer [b->ir]]))

(def performance-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :simplify-result false
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(set-config! performance-config)


(def mch-dir "components/encoder/resources/encoder/")

(def scheduler-ir (b->ir (slurp (str mch-dir "scheduler.mch"))))

(def demo-ir (b->ir (slurp (str mch-dir "demo.mch"))))

(defn time-n
  [ir n]
  (time (boolencode ir
                        :deff-set-size n
                        :max-unroll-size (inc (* n n))))
  n) ;; Don't return the translated Machine, because it is too big. We just want the time.

(defn experiment
  [machine min max step]
  (for [i (range min max step)]
    (time-n machine i)))

(comment

  (experiment scheduler-ir 50002 100002 2000)

  (experiment demo-ir 90002 100002 2000)

)

;; (clojure.pprint/pprint (range 2 100002 2000))
