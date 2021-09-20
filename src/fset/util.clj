(ns fset.util
  (:require [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(defn get-nodes-by-tag
  [el-tag ir]
  (s/select [(s/walker #(= (:tag %) el-tag))] ir))

(defn update-nodes-by-tag
  [tag update-fn ir]
  (s/transform [(s/walker #(= (:tag %) tag))] update-fn ir))
