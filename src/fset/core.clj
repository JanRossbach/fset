(ns fset.core
  (:require
   [clojure.spec.alpha :as spec]
   [fset.spec :refer :all]
   [fset.util :as util]
   [lisb.translation.util :refer [lisb->ir ir->ast ir->b]]
   [lisb.prob.animator :refer [state-space!]]))

;; High level public interface of the app. Ties together the lisb,  config and transform namespaces to provide all required functionality.

(defn make-mch!
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

(defn- remove-old-variables-and-sets
  [mch]
  {:pre [(spec/valid? :fset/mch mch)]
   :post [(spec/valid? :fset/mch %)]}
  (let [{:keys [ir ss meta]} mch]
    {:ir (-> ir util/clear-sets)
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
       (remove-old-variables-and-sets)
       (transform-invariant)
       (transform-init)
       (transform-operations)
       (:ir)
       (ir->b)))
