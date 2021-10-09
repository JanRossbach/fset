(ns fset.backend.variables
  (:require [fset.util :as util]))

(defmulti resolve-type (fn [_ _ t] (:tag t)))

(defmethod resolve-type :default
  [_ var-id _]
  [var-id])

(defmethod resolve-type :power-set
  [u var-id _]
  (let [n (:set-size u)
        set (:target-set u)]
    (vec (map (fn [i] (keyword (str (name var-id) (name set) i))) (range n)))))

(defn- is-relevant?
  [u [_ type]]
  (let [set-to-rewrite (:target-set u)]
    (= (:set type) set-to-rewrite)))

(defn- generate-variable
  [u [var-id type]]
  (let [old-variables (:variables u)
        new-variables (assoc old-variables var-id (resolve-type u var-id type))]
    (assoc u :variables new-variables)))

(defn generate-variables
  "Statically analyzes the IR and generates a map of bindings from variable id's that
  need to be rewritten to coll of boolean ids corresponding to the variable in the new machine."
  [u]
  (let [vars (util/get-vars u)
        typedefs (map (fn [v] [v,(first (util/get-type u v))]) vars)
        relevant-vars (filter (partial is-relevant? u) typedefs)]
    (reduce generate-variable u relevant-vars)))
