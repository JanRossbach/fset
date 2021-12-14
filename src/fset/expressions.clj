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
  (->> ((fn T [e]
          (match e
            #{} (repeat cfg/max-unroll-size {:formula FALSE})
            (enumeration-set :guard set?) (map (fn [e] (if (contains? enumeration-set e) {:formula TRUE :elem e} {:formula FALSE :elem e})) (b/get-type-elems (first enumeration-set)))
            {:tag :union :sets ([A B] :seq)} (m/emap (fn [a b] {:formula (OR (:formula a) (:formula b))
                                                                :elem (:elem a)})
                                                     (T A) (T B))

            {:tag :intersection :sets ([A B] :seq)} (m/emap (fn [a b] {:formula (AND (:formula a) (:formula b))
                                                                       :elem (:elem a)})
                                                            (T A) (T B))

            {:tag :difference :sets ([A B] :seq)} (m/emap (fn [a b] {:formula (AND (:formula a) (NOT (:formula b)))
                                                                     :elem (:elem a)})
                                                          (T A) (T B))
            {:tag :unite-sets :set-of-sets ss} (apply map OR (map T ss))
            {:tag :intersect-sets :set-of-sets ss} (apply map AND (map T ss))
            {:tag :sub :nums ns} (T {:tag :difference :sets ns})

             ;; Relations
            {:tag :inverse :rel r} (m/transpose (T r))
            {:tag :image :rel r :set s} (filter (fn [r] (contains? s) (:elem (first r))) (T r))

            (:or (_ :guard #(and (b/constant? %) (b/unrollable? %))) {:tag (:or :lambda :comprehension-set)})
            (m/emap (fn [elem] {:formula (IN elem e)
                                :elem elem})
                    (b/get-type-elem-matrix e))

            (variable :guard b/variable? b/unrollable?)
            (m/emap (fn [elem] {:formula (=TRUE (b/create-boolname variable elem))
                                :elem elem})
                    (b/get-type-elem-matrix variable))

            _ (throw (ex-info "Unsupported Expression found!" {:expr set-expr :failed-because e}))))
        set-expr)
       flatten
       (sort-by :elem)
       (map :formula)))


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
  (s/transform [(s/walker (and b/unrollable? b/variable?))] boolvar->set ir))
