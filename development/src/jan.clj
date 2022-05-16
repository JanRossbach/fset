(ns jan
  (:require
   [hhu.fset.lib.core :as fset]
   [clojure.pprint :refer [pprint]]
   [hhu.fset.simplifier.interface :refer [simplify-all]]
   [hhu.fset.backend.interface :as b]
   [hhu.fset.encoder.core :refer [unroll-operation]]
   [hhu.fset.encoder.translations :refer [boolvar->set boolvars->set setexpr->bitvector unroll-predicate]]
   [lisb.translation.util :refer [b->ir ir->b]]))

(def jan-config
  {:max-unroll-size 200
   :unroll-invariant true
   :unroll-sub true
   :deff-set-size 2
   :logging false
   :excluded-vars #{}})

(def mch-dir "components/encoder/resources/encoder/")

(defn mch->ir [filename]
  (b->ir (slurp (str mch-dir filename))))

;; Machines

(def test-ir (mch->ir "Test.mch"))
(def scheduler-ir (mch->ir "scheduler.mch"))
(def train-ir (mch->ir "Train.mch"))

(def comb-ir (mch->ir "Combinations.mch"))
(def library-ir (mch->ir "Library.mch"))

(def testb2sat-ir (mch->ir "b2sat_test.mch"))

(pprint testb2sat-ir)

(spit (str mch-dir "/" "b2sat_auto.mch") (ir->b (fset/boolencode testb2sat-ir :logging true :keep-statespace true)))

(unroll-predicate {:tag :member, :elem :x, :set :s})

;; Unroll Numbers

;; Scheduler

(fset/num-vars scheduler-ir) ;; => 3
(fset/num-unrollable-vars scheduler-ir) ;; => 3

(fset/num-ops scheduler-ir) ;; => 5
(fset/num-unrollable-ops scheduler-ir) ;; => 4

;; Train

(fset/num-vars train-ir) ;; => 7
(fset/num-unrollable-vars train-ir) ;; 7

(fset/num-ops train-ir) ;; => 8
(fset/num-unrollable-ops train-ir) ;; => 8

;; Library

(fset/num-vars library-ir) ;; => 4
(fset/num-unrollable-vars library-ir) ;; => 3

(fset/num-ops library-ir) ;; => 10
(fset/num-unrollable-ops library-ir) ;; => 10
