(ns fset.simplifier.interface
  (:require
   [fset.simplifier.core :as core]))

(defn simplify-all [ir]
  (core/simplify-all ir))
