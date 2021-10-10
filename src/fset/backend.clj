(ns fset.backend
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.match :refer [match]]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir ir->ast]]
   [lisb.translation.lisb2ir :refer [band bmember? bcomp-set]]))

(m/set-current-implementation :vectorz) ;; switch to vectorz vectors in order to improve performance

(defn transform-expression
  [u e]
  (let [{:keys [target-set variables]} u]
    (match e
      {:tag :union :sets ([s1 s2] :seq)} e
      _ e)))

(defn transform-predicate
  [u p]
  (let [{:keys [target-set variables]} u]
    (match p
      {:tag :and :predicates ps} {:tag :and :predicates (map (partial transform-predicate u) ps)}
      {:tag :member :element _ :set {:tag :power-set :set target-set}} {}
      {:tag :subset :subset v :set target-set} {}
      {:tag :equal :left l :right #{}} {}
      {:tag :equal :left #{} :right r} {}
      _ p ;; All predicates that don't match a pattern are not changed
      )))

(defn set-elems
  [ir set-id]
  (eval-ir-formula (state-space! (ir->ast ir))
                   (lisb->ir `(bcomp-set [:x] (bmember? :x ~set-id)))))
