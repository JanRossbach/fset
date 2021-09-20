(ns fset.core
  (:require
   [fset.config :as cfg]
   [fset.util :refer [update-nodes-by-tag get-nodes-by-tag]]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.translation.util :refer [lisb->ir ir->ast ir->b]]
   [lisb.prob.animator :refer [state-space!]]))

;; High level public interface of the app. Ties together the lisb,  config and transform namespaces to provide all required functionality.

;; FIXME
(defn- get-set-elements
  [set-identifier machine]
  (let [{:keys [ir ss meta]} machine]
    (eval-ir-formula (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-identifier))))))

(defn- add-meta-data [m]
  (assoc m :meta (-> cfg/meta-data
                      (assoc :sets-to-transform '(:PID)))))

(defn- make-mch!
  ([lisb]
   (let [ir (lisb->ir lisb)]
     (add-meta-data {:ir ir
                     :ss (state-space! (ir->ast ir))}))))

(defn- transform-invariant
  [mch]
  mch)


(defn- transform-init
  [mch]
  mch)

(defn- transform-operations
  [mch]
  mch)

(defn transform
  "Takes lisb code and returns a desettyfied B machine as B string."
  [lisb]
  (->> (make-mch! lisb)
       (transform-invariant)
       (transform-init)
       (transform-operations)
       (:ir)
       (ir->b)))
