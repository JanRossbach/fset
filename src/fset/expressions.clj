(ns fset.expressions
  (:require
   [fset.config :as cfg]
   [clojure.core.matrix :as m]
   [com.rpl.specter :as s]
   [clojure.core.match :refer [match]]
   [fset.dsl :refer [AND OR =TRUE NOT TRUE FALSE BOOL IN CARDINALITY bv->setexpr]]
   [fset.backend :as b]))

(defn setexpr->bitvector
  [set-expr]
  ((fn T [e]
     (match e
       #{} (repeat cfg/max-unroll-size FALSE)
       (enumeration-set :guard set?) (map (fn [e] (if (contains? enumeration-set e) TRUE FALSE)) (b/get-type-elems (first enumeration-set)))
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} (map (fn [a b] (AND a (NOT b))) (T A) (T B))
       {:tag :unite-sets :set-of-sets ss} (apply map OR (map T ss))
       {:tag :intersect-sets :set-of-sets ss} (apply map AND (map T ss))
       {:tag :sub :nums ns} (T {:tag :difference :sets ns})
       {:tag :integer-set} (list e)

       ;; Relations

       {:tag :inverse :rel r} (b/transpose-bitvector (T r))
       {:tag :image :rel (r :guard b/unrollable-var?) :set s} (b/image (T r) s)

       {:tag :comprehension-set} (map (fn [elem] (IN elem e)) (b/get-sub-type-elems e))
       {:tag :lambda} (map (fn [elem] (IN elem e)) (b/get-sub-type-elems e))
       (constant :guard b/constant?) (map (fn [elem] (IN elem constant)) (b/get-sub-type-elems constant))
       (variable :guard b/unrollable-var?) (map =TRUE (map :name (b/unroll-variable variable)))
       _ (throw (ex-info "Expression not supported" {:expr set-expr :failed-because e}))))
   set-expr))

(defn intexpr->intexpr
  [intexpr]
  (match intexpr
    {:tag :cardinality :set s} (CARDINALITY (fn [p] (=TRUE (BOOL p))) (setexpr->bitvector s))
    _ intexpr))

(defn unroll-expression
  [expr]
  (match expr
    (_ :guard b/setexpr?) (setexpr->bitvector expr)
    (_ :guard b/intexpr?) (intexpr->intexpr expr)
    _ expr))

(def boolvar->set (comp bv->setexpr b/unroll-variable))

(defn boolvars->set [ir]
  (s/transform [(s/walker b/unrollable-var?)] boolvar->set ir))
