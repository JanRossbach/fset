(ns fset.core
  (:require
   [clojure.spec.alpha :as spec]
   [fset.util :as util]
   [lisb.translation.util :refer [ir->ast ir->b]]
   [lisb.prob.animator :refer [state-space!]]))

;; High level public interface of the app. Ties together the lisb,  config and transform namespaces to provide all required functionality.

(defrecord machine [ir ^de.prob.statespace.StateSpace ss meta])

(defn- transform-invariant
  [^machine machine]
  machine)

(defn- transform-variables
  [^machine machine]
  (let [{:keys [ir ss meta]} machine
        universe (:universe meta)]
    (->machine (-> ir)
               ss
               meta)))

(defn- transform-init
  [^machine machine]
  machine)

(defn- transform-operations
  [^machine machine]
  machine)

(defn- analyse
  [{:keys [ir ss meta]}]
  (let [new-meta meta]
    (->machine ir ss new-meta)))

(defn transform
  "Takes an IR and returns a desettyfied B machine as string."
  [ir meta]
  {:pre [(spec/valid? :lisb/ir ir) (spec/valid? :fset/meta meta)]
   :post [(spec/valid? :lisb/ir %)]}
  (->> (->machine ir (state-space! (ir->ast ir)) meta)
       (analyse)
       (transform-invariant)
       (transform-init)
       (transform-operations)
       (transform-variables)
       (:ir)))
