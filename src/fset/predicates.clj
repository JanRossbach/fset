(ns fset.predicates
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [fset.expressions :refer [unroll-expression setexpr->bitvector intexpr->intexpr boolvars->set]]
   [fset.dsl :refer [MACHINE AND OR =TRUE <=> NOT TRUE FALSE EQUALS => BOOL BOOLDEFS IF ASSIGN IN FUN SURJECTIVE INJECTIVE BIJECTIVE TOTAL-FUN CARDINALITY bv->setexpr]]
   [fset.backend :as b]))

(defn unroll-predicate
  [pred]
  (try
    ((fn T [e]
       (match e
         ;; equalsity
         {:tag :equals :left l :right #{}} (apply AND (map NOT (T l)))
         {:tag :equals :left #{} :right r} (apply AND (map NOT (T r)))
         {:tag :equals :left (l :guard b/setexpr?) :right (r :guard b/setexpr?)}  (apply AND (map <=> (T l) (T r)))
         {:tag :not-equals :left l :right r} (NOT (T (EQUALS l r)))
         {:tag :subset :sets ([(_ :guard b/unrollable-var?) (_ :guard b/type?)] :seq)} {} ;; An empty map just means delete this thing
         {:tag :subset :sets ([s S] :seq)} (apply AND (map (fn [a b] (=> a b)) (T s) (T S)))
         {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (apply AND (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                                          (map (fn [a b] (=> a b)) Ts TS))))
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
         (apply AND (map (fn [binding] (T (b/apply-binding P binding))) (b/ids->bindings ids)))

         ;; Member
         {:tag :member :elem (_ :guard b/set-element?) :set (_ :guard b/type?)} TRUE
         {:tag :member :elem (el-id :guard b/set-element?) :set v} (first (b/pick-bool-var (setexpr->bitvector v) el-id))

         ;; Concrete Function Types
         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-fn :sets ([A B] :seq)}}
         (FUN (b/unroll-variable-as-matrix v A B))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-fn :sets ([A B] :seq)}}
         (TOTAL-FUN (b/unroll-variable-as-matrix v A B))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-surjection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (FUN em) (SURJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-surjection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (TOTAL-FUN em) (SURJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-injection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (FUN em) (INJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-injection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (TOTAL-FUN em) (INJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :partial-bijection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (FUN em) (BIJECTIVE em)))

         {:tag :member :elem (v :guard b/unrollable-var?) :set {:tag :total-bijection :sets ([A B] :seq)}}
         (let [em (b/unroll-variable-as-matrix v A B)] (AND (TOTAL-FUN em) (BIJECTIVE em)))

         {:tag :member :elem (_ :guard b/unrollable-var?) :set (_ :guard b/type?)} {}
         {:tag :member} e

         ;; Numbers
         {:tag :equals :left l :right r} {:tag :equals :left (intexpr->intexpr l) :right (intexpr->intexpr r)}
         {:tag :less-equals :nums ns} {:tag :less-equals :nums (map intexpr->intexpr ns)}
         {:tag :less :nums ns} {:tag :less :nums (map intexpr->intexpr ns)}
         {:tag :greater :nums ns} {:tag :greater :nums (map intexpr->intexpr ns)}
         {:tag :greater-equals :nums ns} {:tag :greater-equals :nums (map intexpr->intexpr ns)}
         expr (unroll-expression expr)))
     pred)
    (catch Exception _ (boolvars->set pred))))
