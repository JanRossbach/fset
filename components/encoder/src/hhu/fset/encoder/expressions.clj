(ns hhu.fset.encoder.expressions
  (:require
   [clojure.core.matrix :as m]
   [com.rpl.specter :as s]
   [clojure.core.match :refer [match]]
   [hhu.fset.dsl.interface :refer [AND OR =TRUE NOT TRUE FALSE BOOL IN CARDINALITY bv->setexpr]]
   [hhu.fset.backend.interface :as b]))

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
  (flatten
   ((fn T [e]
      (match (if (not (b/contains-vars? e)) (b/eval-constant-formula e) e)
        (elem :guard b/set-element?) (T #{elem})
        #{} (repeat (b/max-unroll-size) FALSE)
        (_ :guard b/carrier?) (repeat (b/max-unroll-size) TRUE)

        (enumeration-set :guard #(and (set? %) (every? b/set-element? %)))
        (vector (mapv (fn [el] (if (contains? enumeration-set el) TRUE FALSE)) (b/get-type-elems (first enumeration-set))))

        (enumeration-relation :guard #(and (set? %) (b/simple-tuple? (first %))))
        (bemap (fn [el] (if (contains? enumeration-relation el) TRUE FALSE)) (b/get-type-elem-matrix (first enumeration-relation)))

        (enumeration-set :guard #(and (set? %) (= 1 (count %)) (every? b/fn-call? %)))
        (T (first enumeration-set)) ;; FIXME HAck ...

            ;; Operators
        {:tag :union :sets ([A B] :seq)} (bmadd (T A) (T B))
        {:tag :intersection :sets ([A B] :seq)} (bemul (T A) (T B))
        {:tag :difference :sets ([A B] :seq)} (bmdiff (T A) (T B))
        {:tag :unite-sets :set-of-sets ss} (apply bmadd (map T ss))
        {:tag :intersect-sets :set-of-sets ss} (apply bemul (map T ss))
        {:tag :sub :nums ns} (T {:tag :difference :sets ns})

             ;; Relations
        {:tag :inverse :rel r} (bmtranspose (T r))
        {:tag :image :rel r :set s} (bmmul (T s) (T r))
        {:tag :fn-call :f f :args ([(args :guard b/set-element?)] :seq)} (T {:tag :image :rel f :set #{args}})
        {:tag :fn-call :f f :args ([args] :seq)} (T {:tag :image :rel f :set args})
        {:tag :dom :rel r} (vector (mapv (fn [row] (apply OR row)) (T r)))
        {:tag :ran :rel r} (T {:tag :dom :rel {:tag :inverse :rel r}})
        {:tag :domain-restriction :set s :rel r} (mapv (fn [el row] (mapv (fn [elem] (AND el elem)) row)) (first (T s)) (T r))
        {:tag :range-restriction :set s :rel r} (bmtranspose (T {:tag :domain-restriction :set s :rel {:tag :inverse :rel r}}))
        {:tag :domain-subtraction :set s :rel r} (mapv (fn [el row] (mapv (fn [elem] (AND (NOT el) elem)) row)) (first (T s)) (T r))
        {:tag :range-subtraction :set s :rel r} (bmtranspose (T {:tag :domain-subtraction :set s :rel {:tag :inverse :rel r}}))

            ;; Variables
        (variable :guard b/unrollable-var?)
        (bemap (fn [elem] (=TRUE (b/create-boolname variable elem))) (b/get-type-elem-matrix variable))

        (c :guard #(and (b/constant? %) (b/unrollable? %)))
        (let [ec (b/eval-constant c)] (bemap (fn [elem] (if (contains? ec elem) TRUE FALSE)) (b/get-type-elem-matrix c)))

        ;; Other Expressions defining sets
        (:or
         {:tag (:or :lambda :comprehension-set)}
         (_ :guard #(and (b/variable? %) (not (b/unrollable-var? %)))))
        (bemap (fn [elem] (IN elem e)) (b/get-type-elem-matrix e))

        _ (throw (ex-info "Unsupported Expression found!" {:expr set-expr
                                                           :failed-because e}))))
    set-expr)))

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
