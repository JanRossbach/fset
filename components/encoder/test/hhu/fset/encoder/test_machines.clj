(ns hhu.fset.encoder.test-machines
  (:require  [lisb.translation.util :refer [b->ir ir->b]]))

(def mch-dir "components/encoder/resources/encoder/test/")

(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(def scheduler (b->ir (slurp (str mch-dir "scheduler.mch"))))
(def numbers (b->ir (slurp (str mch-dir "Numbers.mch"))))
(def banking (b->ir (slurp (str mch-dir "Banking.mch"))))
(def train (b->ir (slurp (str mch-dir "Train.mch"))))
(def test-ir (b->ir (slurp (str mch-dir "Test.mch"))))
