(ns hhu.fset.lib.test-machines
  (:require  [lisb.translation.util :refer [b->ir ir->b]]))

(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(def scheduler (b->ir (slurp "bases/lib/resources/lib/scheduler.mch")))

(def numbers (b->ir (slurp  "bases/lib/resources/lib/Numbers.mch")))

(def banking (b->ir (slurp "bases/lib/resources/lib/Banking.mch")))

(def train (b->ir (slurp "bases/lib/resources/lib/Train.mch")))

(def test-ir (b->ir (slurp "bases/lib/resources/lib/Test.mch")))
