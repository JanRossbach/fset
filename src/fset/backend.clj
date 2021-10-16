(ns fset.backend
  (:require
   [clojure.core.matrix :as m]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir ir->ast lisb->ast ast->lisb]]
   [lisb.translation.lisb2ir :refer [bmember? bcomp-set]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand
   de.prob.statespace.Trace))

(m/set-current-implementation :vectorz) ;; switch to vectorz vectors in order to improve performance

(defn get-statespace
  [ir]
  (let [ss (state-space! (ir->ast ir))
        trace (.addTransitionWith (Trace. ss) "$initialise_machine" [])]
    (.getStateSpace trace)))


(defn set-elems
  [ss set-id]
  (eval-ir-formula ss
                   (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-id)))))

(defn get-type
  [ss var]
  (let [formula-ast (lisb->ast var)
        ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
    (.getType (.typeCheck ss ee))))

(defn get-possible-var-states
  [ss var-id predicate]
  (let [invar-lisb (ast->lisb (ir->ast predicate))]
    (eval-ir-formula ss (lisb->ir `(bcomp-set [~var-id] ~invar-lisb)))))

;; TODO
(defn get-parameter-vars
  [u _]
  (map keyword (set-elems (:statespace u) :PID)))
