(ns hhu.fset.backend.interface
  (:require [hhu.fset.backend.core :as core]
            [hhu.fset.backend.util :as u]
            [hhu.fset.backend.lisb-util :as lu]
            [hhu.fset.backend.specter-util :as su]))

;; Setup

(defonce db (atom {}))

(defn setup-backend
  [ir config]
  (let [new-ir (su/replace-def-sets-with-enum-sets (:deff-set-size config) ir)]
    (reset! db (assoc
                {}
                :ir new-ir
                :ss (lu/get-statespace new-ir)
                :cfg config))
    new-ir))

;; core interface

(defn get-all-bools []
  (let [{:keys [ir ss cfg]} @db]
    (core/get-all-bools ir ss cfg)))

(defn eval-constant [c]
  (let [{:keys [ir ss]} @db]
    (core/eval-constant ss ir c)))

(defn predicate?
  [expr]
  (lu/predicate? (:ss @db) expr))

(defn get-type-elem-matrix
  [expr]
  (core/get-type-elem-matrix (:ss @db) expr))

(defn type? [expr]
  (core/type? (:ir @db) expr))

(defn elem->bools [elem]
  (let [{:keys [ir ss]} @db]
    (core/elem->bools ss ir elem)))

(defn unrollable-var?
  [id]
  (let [{:keys [ir ss cfg]} @db]
    (core/unrollable-var? ir ss cfg id)))

(defn unrollable?
  [id]
  (let [{:keys [ir ss cfg]} @db]
    (core/unrollable? ir ss cfg id)))

(defn fn-call?
  [expr]
  (core/fn-call? expr))

(defn unroll-variable
  [var-id]
  (let [{:keys [ir ss cfg]} @db]
    (core/unroll-variable ir ss cfg var-id)))

(defn get-elem-index [elem-id]
  (core/get-elem-index (:ss @db) elem-id))

(defn op->bindings [op]
  (core/op->bindings (:ss @db) op))

(defn ids->bindings [ir ids]
  (core/ids->bindings ir (:ss @db) ids))

(defn get-type-elems [expr]
  (core/get-type-elems (:ss @db) expr))

(defn get-non-det-guards [op]
  (core/get-non-det-guards op))

;; Lisb interface

(defn model-check [ir]
  (lu/model-check ir))

(defn get-type [formula]
  (lu/get-type (:ss db) formula))

(defn intexpr? [expr]
  (lu/intexpr? (:ss @db) expr))

(defn setexpr? [expr]
  (not (intexpr? expr)))

;; Specter interface

(defn get-vars []
  (su/get-vars (:ir @db)))

(defn get-constants []
  (su/get-constants (:ir @db)))

(defn variable?
  [id]
  (su/variable? (:ir @db) id))

(defn carrier?
  [id]
  (su/carrier? (:ir @db) id))

(defn constant?
  [id]
  (su/constant? (:ir @db) id))

(defn apply-binding [body binding]
  (su/apply-binding body binding))

(defn get-props-as-pred
  []
  (su/get-props-as-pred (:ir @db)))

(defn get-invars-as-pred
  []
  (su/get-invars-as-pred (:ir @db)))

(defn get-props-and-invars-as-pred
  []
  (su/get-props-and-invars-as-pred (:ir @db)))

(defn set-element?
  [elem]
  (su/set-element? (:ir @db) elem))

(defn create-boolname [& ids]
  (apply u/create-boolname ids))
