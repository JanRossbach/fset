(ns hhu.fset.simplifier.interface
  (:require
   [hhu.fset.simplifier.core :as core]))

(defn simplify-all [ir]
  (core/simplify-all ir))
