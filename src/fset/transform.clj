(ns fset.transform
  (:require
   [fset.util :as util]))

;; Pure Namespace which does the unraveling of finite sets on the immediate representation of lisb.

(defn transform-sets
  [m]
  m)

(defn transform-invariant
  [m]
  m)

(defn transform-operations
  [m]
  m)

(defn transform
  [m]
  (->> m
       (transform-sets)
       (transform-invariant)
       (transform-operations)))
