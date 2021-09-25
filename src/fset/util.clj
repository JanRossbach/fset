(ns fset.util
  (:require
   [clojure.spec.alpha :as spec]
   [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(defn get-nodes-by-tag
  [tag ir]
  {:pre [(spec/valid? :lisb/tag tag) (spec/valid? :lisb/ir ir)]
   :post [(spec/coll-of :lisb/clause)]}
  (s/select [(s/walker #(= (:tag %) tag))] ir))

(defn update-nodes-by-tag
  [tag update-fn ir]
  {:pre [(spec/valid? :lisb/tag tag) (spec/valid? :lisb/ir ir)]
   :post [(spec/coll-of :lisb/clause)]}
  (s/transform [(s/walker #(= (:tag %) tag))] update-fn ir))

(defn clear-vars
  [ir]
  (update-nodes-by-tag :variables
                       (fn [m] (assoc m :identifiers '()))
                       ir))
(defn add-vars
  [ir vars]
  (update-nodes-by-tag :variables
                       (fn [m]
                         (assoc m :identifiers (concat (:identifiers m) vars)))
                       ir))
