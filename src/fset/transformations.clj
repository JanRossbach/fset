(ns fset.transformations
  (:require
   [clojure.core.match :refer [match]]
   [fset.backend :as b]))


(defn predicate
  [u p]
  (let [{:keys [target-set variables]} u
        target-set-id (:id target-set)
        target-set-elems (:elems target-set)]
    (match p
      {:tag :and :predicates ps} {:tag :and :predicates (map (partial predicate u) ps)}
      {:tag :member :element e :set {:tag :power-set :set target-set-id}} {:tag :and :predicates (map (fn [id] {:tag :member :element id :set :BOOL}) (get variables e))}
      {:tag :subset :subset v :set target-set-id} {:tag :and :predicates (map (fn [id] {:tag :member :element id :set :BOOL}) (get variables (:identifier v)))}
      {:tag :equal :left l :right #{}} {}
      {:tag :equal :left #{} :right r} {}
      _ p)))


(defn expression
  [u e]
  (let [{:keys [target-set variables]} u]
    (match e
      {:tag :union :sets ([s1 s2] :seq)} e
      _ e)))


(defn- op-predicate
  [u pred]
  pred)

(defn- op-substitution
  [u assign]
  assign)

(defn- select-body
  [pred assign & r]
  (list pred assign))

(defn body
  [u body]
  (match body
    {:tag :select :clauses ([pred assign & r] :seq)} {:tag :select :clauses (apply (partial (select-body pred assign)) r)}
    _ body))
