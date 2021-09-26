(ns fset.core
  (:require
   [clojure.spec.alpha :as spec]
   [fset.spec :refer :all]
   [fset.config :as cfg]
   [fset.util :as util]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.translation.util :refer [lisb->ir ir->ast ir->b]]
   [lisb.prob.animator :refer [state-space!]]))

;; High level public interface of the app. Ties together the lisb,  config and transform namespaces to provide all required functionality.

;; FIXME
(defn- get-set-elements
  [set-identifier machine]
  (let [{:keys [ir ss meta]} machine]
    (eval-ir-formula (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-identifier))))))

(defn- make-mch!
  [lisb meta]
  {:post [(spec/valid? :fset/mch %)]}
  (let [ir (lisb->ir lisb)]
    {:ir ir
     :ss (state-space! (ir->ast ir))
     :meta meta}))

(defn- transform-invariant
  [mch]
  {:pre [(spec/valid? :fset/mch mch)]
   :post [(spec/valid? :fset/mch %)]}
  mch)


;; (def scheduler-mch (make-mch! (read-string (slurp "resources/machines/lisb/source/scheduler.edn")) cfg/meta-data))

(defn- remove-old-variables
  [mch]
  {:pre [(spec/valid? :fset/mch mch)]
   :post [(spec/valid? :fset/mch %)]}
  (let [{:keys [ir ss meta]} mch]
    {:ir (-> ir util/clear-vars)
     :ss ss
     :meta meta}))

(defn- transform-init
  [mch]
  {:pre [(spec/valid? :fset/mch mch)]
   :post [(spec/valid? :fset/mch %)]}
  mch)

(defn- transform-operations
  [mch]
  {:pre [(spec/valid? :fset/mch mch)]
   :post [(spec/valid? :fset/mch %)]}
  mch)

(defn transform
  "Takes lisb code and returns a desettyfied B machine as string."
  [meta lisb]
  (->> (make-mch! lisb meta)
       (transform-invariant)
       (remove-old-variables)
       (transform-init)
       (transform-operations)
       (:ir)
       (ir->b)))
