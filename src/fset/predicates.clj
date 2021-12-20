(ns fset.predicates
  (:require
   [fset.config :as cfg]
   [clojure.core.match :refer [match]]
   [fset.expressions :refer [unroll-expression boolvars->set]]
   [fset.dsl :refer [AND OR <=> NOT =TRUE TRUE EQUALS => FUN SURJECTIVE INJECTIVE BIJECTIVE TOTAL-FUN BOOL]]
   [fset.backend :as b]))


(defn unroll-predicate
  [pred]
  (try
    ((fn T [e]
       (match e
         ;; equalsity
         {:tag :equals :left l :right #{}} (apply AND (map NOT (T l)))
         {:tag :equals :left #{} :right r} (apply AND (map NOT (T r)))

         {:tag :not-equals :left l :right r} (NOT (T (EQUALS l r)))
         {:tag :subset :sets ([(_ :guard b/unrollable-var?) (_ :guard b/type?)] :seq)} {} ;; An empty map just means delete this thing
         {:tag :subset :sets ([s S] :seq)} (apply AND (map (fn [a b] (=> a b)) (T s) (T S)))
         {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (apply AND (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                                          (map (fn [a b] (=> a b)) Ts TS))))

         {:tag :equals :left (l :guard b/setexpr?) :right (r :guard b/setexpr?)}  (apply AND (map <=> (T l) (T r)))
         ;; logical operators
         {:tag :and :preds ps} (apply AND (map T ps))
         {:tag :or :preds ps} (apply OR (map T ps))
         {:tag :not :pred p} (NOT (T p))
         {:tag :equivalence :preds ([A B] :seq)} (<=> (T A) (T B))
         {:tag :implication :preds ([A B] :seq)} (=> (T A) (T B))

         ;; Quantifiers
         {:tag :for-all :ids ids :implication {:tag :implication :preds ([P Q] :seq)}}
         (apply AND (map (fn [binding] (=> (T (b/apply-binding P binding))
                                           (T (b/apply-binding Q binding))))
                         (b/ids->bindings P ids)))

         {:tag :exists :ids ids :pred P}
         (apply AND (map (fn [binding] (T (b/apply-binding P binding))) (b/ids->bindings P ids)))

         ;; Member
         {:tag :member :elem (_ :guard b/unrollable-var?) :set (_ :guard b/type?)} {}
         {:tag :member :elem {:tag :fn-call :f f :args ([(arg :guard b/set-element?)] :seq)} :set S} (T {:tag :subset :sets (list {:tag :image :rel f :set #{arg}} S)})
         {:tag :member :elem (elem :guard b/set-element?) :set s} (=TRUE (BOOL (nth (T s) (b/get-elem-index elem))))

         ;; Concrete Function Types
         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-fn :sets ([_ _] :seq)}}
         (FUN (b/get-type-elem-matrix v))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-fn :sets ([_ _] :seq)}}
         (TOTAL-FUN (b/get-type-elem-matrix v))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-surjection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (FUN em) (SURJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-surjection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (TOTAL-FUN em) (SURJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-injection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (FUN em) (INJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-injection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (TOTAL-FUN em) (INJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-bijection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (FUN em) (BIJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-bijection :sets ([_ _] :seq)}}
         (let [em (b/get-type-elem-matrix v)] (AND (TOTAL-FUN em) (BIJECTIVE em)))

         ;; Numbers
         {:tag :equals :left l :right r} {:tag :equals :left (T l) :right (T r)}
         {:tag :less-equals :nums ns} {:tag :less-equals :nums (map T ns)}
         {:tag :less :nums ns} {:tag :less :nums (map T ns)}
         {:tag :greater :nums ns} {:tag :greater :nums (map T ns)}
         {:tag :greater-equals :nums ns} {:tag :greater-equals :nums (map T ns)}

         expr (unroll-expression expr)))
     pred)
    (catch Exception e
      (cfg/log e pred)
      (boolvars->set pred))))
