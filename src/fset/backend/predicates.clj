(ns fset.backend.predicates
  (:require
   [clojure.core.match :refer [match]]))

(defmulti transform-predicate "Takes the universe and a predicate and returns the transformed predicates. Dispatches on the tag of the predicate."
  (fn [_ p] (:tag p)))

(defmethod transform-predicate :default
  [_ p]
  p)

(defmethod transform-predicate :and
  [u p]
  {:tag :and
   :predicates (map (partial transform-predicate u) (:predicates p))})

(defmethod transform-predicate :member
  [u p]
  (let [{:keys [element set]} p
        set-id (:set set)
        target-set (:target-set u)]
    (if (= set-id target-set)
      (match set
             {:tag :power-set} {}
             _ p)
      p)))
