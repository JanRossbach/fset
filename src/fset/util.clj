(ns fset.util
  (:require
   [com.rpl.specter :as s]))

;; Namespace to provide specter functionality to the transform ns.

(defn get-nodes-by-tag
  [tag ir]
  (s/select [(s/walker #(= (:tag %) tag))] ir))

(defn update-nodes-by-tag
  [tag update-fn ir]
  (s/transform [(s/walker #(= (:tag %) tag))] update-fn ir))

(defn clear-vars
  [ir]
  (update-nodes-by-tag :variables
                       (fn [m] (assoc m :identifiers nil))
                       ir))

(defn add-identifiers
  [ids m]
  (let [current-ids (:identifiers m)]
    (if (nil? current-ids)
      (assoc m :identifiers ids)
      (assoc m :identifiers (concat current-ids ids)))))

(defn add-clause
  [ir new-clause]
  (let [clauses (:clauses ir)
        new-clauses (cons new-clause clauses)]
    (assoc ir :clauses new-clauses)))

(defn add-vars
  [ir vars]
  (if (and (empty? (get-nodes-by-tag :variables ir)) (seq vars))
    (add-clause ir {:tag :variables
                    :identifiers vars})
    (update-nodes-by-tag :variables
                         (partial add-identifiers vars)
                         ir)))
