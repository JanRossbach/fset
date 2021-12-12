(ns fset.test-machines
  (:require
   [lisb.translation.util :refer [b->ir]]))

(def empty-ir {:tag :machine, :clauses '(), :name :Empty})

(def scheduler (b->ir (slurp "resources/test/scheduler.mch")))

(def numbers (b->ir (slurp "resources/test/Numbers.mch")))

(def scheduler-transformed (read-string (slurp "resources/test/scheduler-ir.edn")))

(def banking (b->ir (slurp "resources/test/Banking.mch")))

(def train (b->ir (slurp "resources/test/Train.mch")))

(def test-ir (b->ir (slurp "resources/test/Test.mch")))
