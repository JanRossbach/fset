(ns fset.transformations
  (:require
   [clojure.core.match :refer [match]]
   [lisb.translation.util :refer [ir->b lisb->ir]]))

(defn gen-bool-typedefs
  [elems]
  {:tag :and
   :predicates (map (fn [el] {:tag :member :element el :set :BOOL}) elems)})

(defn type?
  [expr]
  (match expr
    (_ :guard keyword?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin-set :set (_ :guard type?)} true
    {:tag :fin1-set :set (_ :guard type?)} true
    _ false))

(defn typedef? [expr]
  (match expr
         {:tag :member :element (_ :guard keyword?) :set (_ :guard type?)} true
         {:tag :subset :subset (_ :guard keyword?) :set (_ :guard type?)} true
         _ false))


;; NOTE: POW(EXPR) is not supported, since the powerset can not be factored out POW(A /\ B) /= POW(A) /\ POW(B)
;; NOTE: Cardinality not yet supported, because it's a pain in the ass...

(defn predicate
  "Takes a B Predicate, that is not a type definition, and a collection of elements, and returns
  a collection of predicates that represent the unrolled expression."
  [expr elems]
  ((fn T [e]
     (match e
       ;; equality
       {:tag :equal :left l :right #{}} (list {:tag :and :predicates (map (fn [p] {:tag :not :predicate p}) (T l))})
       {:tag :equal :left #{} :right r} (list {:tag :and :predicates (map (fn [p] {:tag :not :predicate p}) (T r))})
       {:tag :equal :left l :right r} (list {:tag :equivalence :predicates (list {:tag :and :predicates (T l)} {:tag :and :predicates (T r)})})
       {:tag :not-equal :left l :right r} (list {:tag :not :predicate (T (first {:tag :equal :left l :right r}))})
       ;; logical operators
       {:tag :and :predicates ps} (list {:tag :and :predicates (mapcat T ps)})
       {:tag :or :predicates ps} (list {:tag :or :predicates (mapcat T ps)})
       {:tag :not :predicate p} (map #(constantly {:tag :not :predicate %}) (T p))
       {:tag :equivalence :predicates ([A B] :seq)} (list {:tag :equivalence :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})
       {:tag :implication :predicates ([A B] :seq)} (list {:tag :implication :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})

       ;; binary-set-operators
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] {:tag :or :predicates (list a b)}) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] {:tag :and :predicates (list a b)}) (T A) (T B))
       {:tag :general-union :set-of-sets ss} (apply (partial map (fn [& s] {:tag :or :predicates s})) (map T ss))
       {:tag :general-intersection :set-of-sets ss} (apply (partial map (fn [& s] {:tag :and :predicates s})) (map T ss))
       {:tag :difference :sets ([A B] :seq)} {:tag :and :predicates (list (T A) {:tag :not :predicate (T B)})}

       ;; leaf node
       (A :guard type?) (map (fn [x] {:tag :member :element x :set A}) elems))) expr))
