(ns dev.jan
  (:require
   [hhu.fset.lib.core :as fset]
   [taoensso.timbre :as log]
   [clojure.pprint :refer [pprint]]
   [hhu.fset.backend.interface :as b]
   [lisb.translation.util :refer [b->ir ir->b]]))


(def jan-config
  {:max-unroll-size 200
   :eval-constants true
   :unroll-invariant true
   :simplify-result true
   :deff-set-size 2
   :logging true
   :excluded-vars #{}})

(comment

  (fset/set-config! jan-config)

  (fset/reset-config)

  (def scheduler-ir (b->ir (slurp "components/encoder/resources/encoder/scheduler.mch"))) ;; Read in the B machine IR from a file

  (def scheduler-auto-ir (fset/boolencode scheduler-ir))

  (pprint scheduler-auto-ir)

  (time (fset/boolencode scheduler-ir))

  scheduler-auto-ir

  (ir->b scheduler-auto-ir)

 (b/model-check (b->ir (ir->b (fset/boolencode scheduler-ir))))

  (spit "resources/test/scheduler-ir.edn" (fset/boolencode scheduler-ir))

  (def train-ir (b->ir (slurp "components/encoder/resources/encoder/Train.mch")))

  (def train-auto-ir (fset/boolencode train-ir :excluded-vars #{:TRK :rsrtbl} :logging true))

  (spit "components/encoder/resources/encoder/train_auto1.mch" (ir->b train-auto-ir)) ;; Write the translated IR to another file

  )
