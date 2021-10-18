(ns fset.transformations
  (:require
   [clojure.core.match :refer [match]]
   [fset.backend :as b]
   [lisb.translation.util :refer [lisb->ir]]
   [fset.util :as util]))

(defn bool-typedefs
  [u var]
  (let [elems (util/get-var-elems u var)]
    {:tag :and
     :predicates (lisb->ir
                  `(map #(lisb.translation.lisb2ir/bmember? % :BOOL) ~elems))}))

(defn predicate
  [u p]
  (let [ids (util/get-target-set-ids u)
        id (first ids)]
    (match p
      {:tag :and :predicates ps} {:tag :and :predicates (map (partial predicate u) ps)}
      {:tag :member :element e :set {:tag :power-set :set id}} (bool-typedefs u e)
      {:tag :subset :subset v :set id} (bool-typedefs u v)
      {:tag :equal :left l :right #{}} {}
      {:tag :equal :left #{} :right r} {}
      _ p)))

(defn variable?
  [u id]
  (some #(= % id) (util/get-vars u)))

(defn union
  [_ s1 s2]
  {:tag :union
   :sets (list s1 s2)})

(defn expression
  [u e]
  (let [set-ids (util/get-set-ids u)]
    (match e
           {:tag :union :sets ([s1 s2] :seq)} (union u s1 s2)
           _ e)))

(expression {} :active)

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
    {:tag :select :clauses ([pred assign & r] :seq)} {:tag :select :clauses (apply (partial select-body pred assign) r)}
    _ body))

(defn substitution
  [u sub]
  (match sub
    {:tag :parallel-substitution :substitutions s} {:tag :parallel-substitution
                                                    :substitutions (map (partial substitution u) s)}
    _ sub))
