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

(defn unroll-predicate
  "Takes a B predicate of type POW(S), that is not a type definition, and a collection of elements, and returns
  a collection of predicates that represent the unrolled expression."
  [expr elems]
  ((fn T [e]
     (match e
       ;; equality
       {:tag :equal :left l :right #{}} (map (fn [p] {:tag :not :predicate p}) (T l))
       {:tag :equal :left #{} :right r} (map (fn [p] {:tag :not :predicate p}) (T r))
       {:tag :equal :left l :right r} (map #(constantly {:tag :equivalence :predicates (list %1 %2)}) (T l) (T r))
       {:tag :not-equal :left l :right r} (map #(constantly {:tag :not :predicate %}) (T {:tag :equal :left l :right r}))
       {:tag :subset :subset s :set S} (map #(constantly {:tag :implication :predicates (list %1 %2)}) (T s) (T S))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons {:tag :or :predicates (map (fn [a b] {:tag :and :predicates (list {:tag :not :predicate a} b)}) Ts TS)}
                                                                             (map (fn [a b] {:tag :implication :predicates (list a b)}) Ts TS)))
       ;; logical operators
       {:tag :and :predicates ps} (list {:tag :and :predicates (mapcat T ps)})
       {:tag :or :predicates ps} (list {:tag :or :predicates (mapcat T ps)})
       {:tag :not :predicate p} (map #(constantly {:tag :not :predicate %}) (T p))
       {:tag :equivalence :predicates ([A B] :seq)} (list {:tag :equivalence :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})
       {:tag :implication :predicates ([A B] :seq)} (list {:tag :implication :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})

       ;; binary-set-operators
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] {:tag :or :predicates (list a b)}) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] {:tag :and :predicates (list a b)}) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} {:tag :and :predicates (list (T A) {:tag :not :predicate (T B)})}
       {:tag :member :element x :set s} (T {:tag :subset :subset x :set (:set s)})

       ;; general-set-operators
       {:tag :general-union :set-of-sets ss} (apply (partial map (fn [& s] {:tag :or :predicates s})) (map T ss))
       {:tag :general-intersection :set-of-sets ss} (apply (partial map (fn [& s] {:tag :and :predicates s})) (map T ss))

       ;; leaf node
       (A :guard type?) (map (fn [x] {:tag :member :element x :set A}) elems))) expr))

(defn unroll-substitution
  [expr elems]
  ((fn T [e]
     (match e
            {:tag :assign :identifiers ids :values vals} {}

            )) expr))
