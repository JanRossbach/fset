(ns hhu.fset.encoder.expressions
  (:require
   [clojure.core.matrix :as m]
   [com.rpl.specter :as s]
   [clojure.core.match :refer [match]]
   [hhu.fset.encoder.predicates :refer [unroll-predicate]]
   [hhu.fset.dsl.interface :refer [AND OR =TRUE NOT TRUE FALSE BOOL IN CARDINALITY bv->setexpr EQUALS]]
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
  "
  Takes a set expression and returns a predicate sequence or cardinality |Type(set-expr)|.
  If the predicate evaluates to true, the element is in the given set.
  If the given IR expression is infinite or not supported, an exception is thrown.

  In: IR map representing a set expression.
  Out: IR pred seq
  "
  [set-expr]
  (flatten
   ((fn T [e]
      (match (if (not (b/contains-vars? e)) (b/eval-constant-formula e) e)
        (set-elem :guard b/set-element?) (T #{set-elem})
        #{} (repeat (b/max-unroll-size) FALSE)
        (_ :guard b/carrier?) (repeat (b/max-unroll-size) TRUE)

        {:tag :comprehension-set :ids ([id] :seq) :pred pred} (bemap (fn [elem] (unroll-predicate (b/apply-binding pred [[id elem]]))) (b/get-type-elem-matrix e))

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
        {:tag :cartesian-product :sets sets} (reduce bmadd (map T sets))
        {:tag :id :set sete} (bemap (fn [{:keys [left right]}] (if (= left right) TRUE FALSE)) (b/get-type-elem-matrix sete))
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
        {:tag :composition :rels rels} (reduce bmmul (map T rels))
        {:tag :iterate :rel rel :num num} (nth (iterate (partial bmmul rel) rel) num)
        {:tag :override :rels ([A B] :seq)} (T {:tag :union :sets [B {:tag :domain-subtraction :set {:tag :dom :rel B} :rel A}]})
        {:tag :lambda :ids ([id] :seq) :pred pred :expr expr} (bemap (fn [{:keys [left right]}] (AND
                                                                                                 (unroll-predicate (b/apply-binding pred [[id left]]))
                                                                                                 (nth (T expr) (b/get-elem-index right))))
                                                                     (b/get-type-elem-matrix e))
        ;; Variables
        (variable :guard b/unrollable-var?)
        (bemap (fn [elem] (=TRUE (b/create-boolname variable elem))) (b/get-type-elem-matrix variable))
        (variable :guard b/variable?)
        (bemap (fn [elem] (EQUALS variable elem)) (b/get-type-elem-matrix variable))

        (c :guard #(and (b/constant? %) (b/unrollable? %)))
        (let [ec (b/eval-constant c)] (bemap (fn [elem] (if (contains? ec elem) TRUE FALSE)) (b/get-type-elem-matrix c)))

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
