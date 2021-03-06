(ns hhu.fset.backend.interface
  (:require
   [lisb.prob.animator :refer [get-result]]
   [hhu.fset.backend.core :as core]
   [hhu.fset.backend.lisb-util :as lu]
   [hhu.fset.backend.specter-util :as su]
   [hhu.fset.backend.util :as u]))

;; Setup

(defonce db (atom {}))

(defn make-set [x]
  (if (coll? x)
    (set x)
    #{x}))

(defn kill-statespace! [ir config]
  (let [ss (:ss @db)
        curr-ir (:ir @db)]
    (if (not (or (:keep-statespace config)
                 (nil? ss)
                 (= ir curr-ir)))
      (.kill ss) nil)))

(defn setup-backend
  [ir config]
  (kill-statespace! ir config)
  (let [new-ir (su/replace-def-sets-with-enum-sets (:deff-set-size config) ir)
        ss (lu/get-statespace new-ir)]
    (.explore (.getState ss "root"))
    (let [cs (.getDestination (first (.getTransitions (.getState ss "root"))))
          constants (.getConstantValues
                     cs
                     de.prob.animator.domainobjects.FormulaExpand/EXPAND)]
      (reset! db (assoc
                  {}
                  :ir new-ir
                  :ss ss
                  :constant-state cs
                  :constants (into {} (for [[id-obj v-obj] constants] [(keyword (.toString id-obj)) (make-set (core/interpret-animator-result (get-result v-obj)))]))
                  :cfg config)))
    new-ir))

(defn max-unroll-size []
  (:max-unroll-size (:cfg @db)))

(defn unroll-invariant? []
  (:unroll-invariant (:cfg @db)))

(defn unroll-sub? []
  (:unroll-sub (:cfg @db)))

;; core interface

(defn get-all-bools []
  (let [{:keys [ir ss cfg]} @db]
    (core/get-all-bools ir ss cfg)))

(defn eval-constant [c]
  (get (:constants @db) c))

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

(defn unrollable-op?
  [op]
  (core/unrollable-op? (:ss @db) op))

(defn unrollable-param?
  [op id]
  (core/unrollable-param? (:ir @db) (:ss @db) op id))

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
  (lu/get-type (:ss @db) formula))

(defn intexpr? [expr]
  (lu/intexpr? (:ss @db) expr))

(defn setexpr? [expr]
  (not (intexpr? expr)))

(defn eval-constant-formula [ir-formula]
  (let [res (core/interpret-animator-result (lu/eval-constant-formula (:constant-state @db) ir-formula))]
    (if (coll? res) (into #{} res) #{res})))

;; Specter interface

(defn contains-vars? [expr]
  (su/contains-vars? (:ir @db) expr))

(defn get-vars []
  (su/get-vars (:ir @db)))

(defn get-ops []
  (su/get-ops (:ir @db)))

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

(defn simple-tuple?
  [elem]
  (su/simple-tuple? (:ir @db) elem))

(defn create-boolname [& ids]
  (apply u/create-boolname ids))

(defn num-ops []
  (count (su/get-ops (:ir @db))))

(defn num-vars []
  (count (su/get-vars (:ir @db))))

(defn num-unrollable-vars []
  (let [{:keys [ir ss cfg]} @db]
    (count (filter (partial core/unrollable? ir ss cfg) (su/get-vars ir)))))

(defn num-unrollable-ops []
  (let [{:keys [ir ss]} @db]
    (count (filter (partial core/unrollable-op? ss) (su/get-ops ir)))))
