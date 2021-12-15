(ns fset.expressions
  (:require
   [fset.config :as cfg]
   [clojure.core.matrix :as m]
   [com.rpl.specter :as s]
   [clojure.core.match :refer [match]]
   [fset.dsl :refer [AND OR =TRUE NOT TRUE FALSE BOOL IN CARDINALITY bv->setexpr]]
   [fset.backend :as b]))


(defn bmtranspose [bm]
  (apply (partial mapv vector) bm))

(defn bmadd [& bms]
  (apply m/emap (fn [& bs] (apply OR bs)) bms))

(defn bemul [& bms]
  (apply m/emap (fn [& bs] (apply AND bs)) bms))

(defn bmdiff [& bms]
  (apply m/emap (fn [a & bs] (apply AND a (map NOT bs))) bms))

(defn bmmul [a b]
  (let [nested-for (fn [f x y] (mapv (fn [a] (mapv (fn [b] (f a b)) y)) x))]
    (nested-for (fn [x y] (apply OR (mapv AND x y))) a (bmtranspose b))))

(defn bemap [f & bms]
  (apply mapv (fn [& rows] (apply mapv (fn [& elems] (apply f elems)) rows)) bms))

(defn setexpr->bitvector
  [set-expr]
  (->> ((fn T [e]
          (match e
            #{} (repeat cfg/max-unroll-size FALSE)

            (enumeration-set :guard set?)
            (vector (mapv (fn [e] (if (contains? enumeration-set e) TRUE FALSE)) (b/get-type-elems (first enumeration-set))))

            {:tag :union :sets ([A B] :seq)} (bmadd (T A) (T B))
            {:tag :intersection :sets ([A B] :seq)} (bemul (T A) (T B))
            {:tag :difference :sets ([A B] :seq)} (bmdiff (T A) (T B))
            {:tag :unite-sets :set-of-sets ss} (apply bmadd (map T ss))
            {:tag :intersect-sets :set-of-sets ss} (apply bemul (map T ss))
            {:tag :sub :nums ns} (T {:tag :difference :sets ns})

             ;; Relations
            {:tag :inverse :rel r} (bmtranspose (T r))
            {:tag :image :rel r :set s} (bmmul (T s) (T r))

            (:or (_ :guard #(and (b/constant? %) (b/unrollable? %))) {:tag (:or :lambda :comprehension-set)})
            (bemap (fn [elem] (IN elem e)) (b/get-type-elem-matrix e))

            (variable :guard b/variable? b/unrollable?)
            (bemap (fn [elem] (=TRUE (b/create-boolname variable elem))) (b/get-type-elem-matrix variable))

            _ (throw (ex-info "Unsupported Expression found!" {:expr set-expr :failed-because e}))))
        set-expr)
       flatten))


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
