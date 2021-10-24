(ns fset.unroll
  (:require
   [fset.backend :as b]
   [fset.util :as u]
   [fset.transformations :as transform]))

(defn new-op
  [old-op param-id]
  (let [{:keys [name body]} old-op]
    {:tag :operation
     :name (keyword (str (clojure.core/name name) "_" (clojure.core/name param-id)))
     :parameters []
     :body (transform/op-body body param-id)}))

(defn unroll-operation
  [ir op-name]
  (let [op (u/get-op ir op-name)
        ss (b/get-statespace ir)
        {:keys [return parameters body]} op
        vars (u/get-vars ir)
        param-vals (b/get-possible-var-states ss parameters vars)]
    (u/add-operations ir (map (partial new-op op) param-vals))))
