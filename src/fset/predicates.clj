(ns fset.predicates)

(defmulti transform-predicate "Takes the universe and a predicate and returns the transformed predicates. Dispatches on the tag of the predicate."
  (fn [_ p] (:tag p)))

(defmethod transform-predicate :default
  [_ p]
  p)

(defmethod transform-predicate :and
  [u p]
  {:tag :and
   :predicates (map (partial transform-predicate u) (:predicates p))})
