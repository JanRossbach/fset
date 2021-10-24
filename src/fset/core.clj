(ns fset.core
  (:require
   [potemkin :refer [import-vars]]
   [fset.util :as u]))

(import-vars [fset.varencode varencode valid-var?])
(import-vars [fset.unroll unroll-operation])


;; Var-encode Frontend

(def default-config
  {:max-size 10
   :def-set-size 3
   :debug true})

(defn encode-all
  [ir]
  (let [vars (u/get-vars)
        valid-vars (filter (partial valid-var? ir) vars)]
    (reduce varencode ir valid-vars)))

;; Unroll Frontend

(defn unroll-all
  [ir]
  (let [ops (u/get-operations)]
    (reduce unroll-operation ir ops)))
