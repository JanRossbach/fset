(ns fset.backend
  (:require
   [fset.config :as cfg]
   [clojure.core.matrix :as m]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [ir->ast b->ir]]
   [lisb.translation.lisb2ir :refer [bmember? bcomprehension-set bexists band b=]])
  (:import
   de.prob.animator.domainobjects.ClassicalB
   de.prob.animator.domainobjects.FormulaExpand))


;; SETUP

(defn- TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:clauses]))
(defn- CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def VARIABLES (s/path [(CLAUSE :variables) :values]))
(def INVARIANTS (s/path [(CLAUSE :invariants) :values]))
(def PROPERTIES (s/path [(CLAUSE :properties) :values]))
(def CONSTANTS (s/path [(CLAUSE :constants) :values]))
(def SETS (s/path [(CLAUSE :sets) :values s/ALL]))
(defn OPERATION [name] (s/path [(CLAUSE :operations) :values s/ALL #(= (:name %) name)]))

(declare get-statespace get-max-size create-boolname)

(def db (atom {}))

(defn get-vars []
  (s/select [VARIABLES s/ALL] (:ir @db)))

(defn get-constants []
  (s/select [CONSTANTS s/ALL] (:ir @db)))

(defn variable?
  [id]
  (seq (s/select [VARIABLES s/ALL #(= % id)] (:ir @db))))

(defn constant?
  [id]
  (seq (s/select [CONSTANTS s/ALL #(= % id)] (:ir @db))))

(def get-type
  (memoize
   (fn [formula]
     (let [ss (:ss @db)
           formula-ast (ir->ast formula)
           ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
       (.getType (.typeCheck ss ee))))))

(def get-statespace
  (memoize (fn [ir] (state-space! (ir->ast ir)))))

(defn typestring->ir
  [type-str]
  (let [query-str (str "MACHINE scheduler\nCONSTANTS x\nPROPERTIES x:" type-str "\nEND")
        query-ir (b->ir query-str)]
    (first (s/select [(CLAUSE :properties) :values s/FIRST :set] query-ir))))

(def get-type-ir (memoize (comp typestring->ir get-type)))


(defn interpret-animator-result
  [result]
  (if (= result :timeout)
    :timeout
    (match (first result)
      (_ :guard string?) (sort (map keyword result))
      (_ :guard vector?) (sort-by first (map (fn [v] (mapv keyword v)) result))
      (_ :guard set?) (map interpret-animator-result result)
      nil '())))

(defn interpret-animator-result2
  [result]
  (s/transform [(s/walker string?)] keyword result))

(defn- replace-param
  [ir [parameter element]]
  (s/setval [(s/walker #(= % parameter))] element ir))

(defn apply-binding
  [body binding]
  (reduce replace-param body binding))

(defn comprehend
  [ids pred]
  (interpret-animator-result
   (eval-ir-formula
    (:ss @db)
    (bcomprehension-set ids pred))))

(defn get-set-elems
  [set-ir]
  (comprehend [:x] {:tag :member :elem :x :set set-ir}))

(defn get-relation-elems
  [rdomain rrange]
  (comprehend [:x :y] (band (bmember? :x rdomain)
                            (bmember? :y rrange))))

(defn unrollable-pred? [p]
  true)

(def get-type-elems (comp get-set-elems get-type-ir))

(defn get-sub-type-elems
  [expr]
  (match (get-type-ir expr)
         {:tag :power-set :set s} (get-set-elems s)
         {:tag :relation :sets ([A B] :seq)} (get-relation-elems A B)))

(defn predicate?
  [ir]
  (= "predicate" (get-type ir)))

(defn get-props-as-pred
  []
  (let [props (s/select [PROPERTIES s/ALL] (:ir @db))]
    (if (seq props)
      (apply band props)
      {:tag :and :preds '()})))

(defn get-invars-as-pred
  []
  (let [invars (s/select [INVARIANTS s/ALL] (:ir @db))]
    (if (seq invars)
      (apply band invars)
      {:tag :and :preds '()})))

(defn get-props-and-invars-as-pred
  []
  (let [props (:preds (get-props-as-pred))
        invars (:preds (get-invars-as-pred))]
    {:tag :and
     :preds (concat props invars)}))

(defn eval-constant [c]
  (comprehend [:x] (bexists (get-constants) (band (b= :x c) (get-props-as-pred)))))

(defn- involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn- deff-set->enum-set
  [size {:keys [id]}]
  {:tag :enumerated-set
   :id id
   :elems (map (fn [i] (create-boolname (first (name id)) i)) (range 1 (inc size)))})

(defn replace-def-sets-with-enum-sets
  [size ir]
  (s/transform [SETS (TAG :deferred-set)] (partial deff-set->enum-set size) ir))

(defn- non-det-clause?
  [pattern]
  (match pattern
    {:tag :any} true
    _ false))

(defn- get-non-det-clauses
  [op]
  (s/select (s/walker non-det-clause?) op))

(defn get-non-det-guards
  [op]
  (let [non-det-clauses (get-non-det-clauses op)]
    (map (fn [clause]
           (match clause
             {:tag :any :pred w} w))
         non-det-clauses)))

(defn get-non-det-ids
  [op]
  (mapcat (fn [clause]
         (match clause
           {:tag :any :ids ids} ids))
       (get-non-det-clauses op)))

(defn get-max-size [] (:max-size @db))

(defn setup-backend ;; Call this before testing the backend isolated without an actual translation
  [ir]
  (let [new-ir (replace-def-sets-with-enum-sets cfg/deff-set-size ir)]
    (reset! db (assoc
                {}
                :ir new-ir
                :ss (get-statespace new-ir)))
    new-ir))

(defn carrier?
  [id]
  (seq (s/select [SETS :id #(= % id)] (:ir @db))))

(defn type?
  [expr]
  (match expr
    (_ :guard carrier?) true
    {:tag :relation :sets ([_ _] :seq)} true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin :set (_ :guard type?)} true
    {:tag :fin1 :set (_ :guard type?)} true
    _ false))

(defn create-boolname [& ids]
  (keyword (apply str (map (fn [id] (if (keyword? id) (name id) id)) (flatten ids)))))

(defn set-element?
  [id]
  (seq (s/select [SETS :elems s/ALL #(= % id)] (:ir @db))))

(defn finite-type?
  [expr]
  (if (carrier? expr)
    true
    (match (get-type-ir expr)
      {:tag :power-set :set (_ :guard finite-type?)} true
      {:tag :integer-set} false
      {:tag :relation :sets ([A B] :seq)} (and (carrier? A) (carrier? B) (< (count (get-relation-elems A B)) cfg/max-unroll-size))
      _ false)))

(defn finite?
  [expr]
  (match expr
    {:tag :interval :from n :to m} (< (- m n) cfg/max-unroll-size)
    {:tag :cardinality} true
    _ (finite-type? expr)))

(def unrollable-var?
  (memoize
   (fn [var-id]
     (and (not (contains? cfg/excluded-vars var-id))(variable? var-id) (finite? var-id)))))

(defn unroll-variable
  [var-id]
  (if (unrollable-var? var-id)
    (map (fn [elem] {:name (create-boolname var-id elem)
                    :elem elem
                    :var var-id}) (get-sub-type-elems var-id))
    (list {:name var-id :elem nil :var var-id})))

(defn unroll-variable-as-matrix
  [var-id rdom rran]
  (let [dom-elems (get-sub-type-elems rdom)
        ran-elems (get-sub-type-elems rran)]
    (m/matrix (for [d dom-elems]
                (for [r ran-elems]
                  (create-boolname var-id d r))))))

(defn get-all-bools
  []
  (let [vars (s/select [VARIABLES s/ALL] (:ir @db))]
    (filter #(not (variable? (:name %))) (mapcat unroll-variable vars))))

(defn elem->bools
  [elem]
  (let [elem-type (typestring->ir (get-type elem))
        vars (first (s/select [VARIABLES] (:ir @db)))]
    (mapcat
     (fn [v]
       (match (typestring->ir (get-type v))
         {:tag :power-set :set (_ :guard #(= % elem-type))} (list (create-boolname v elem))
         {:tag :relation :sets ([(_ :guard #(= % elem-type)) B] :seq)} (map (fn [el2] (create-boolname v elem el2)) (get-set-elems B))
         {:tag :relation :sets ([A (_ :guard #(= % elem-type))] :seq)} (map (fn [el1] (create-boolname v el1 elem)) (get-set-elems A))
         _ '()))
     vars)))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (elem->bools el-id))) formulas))

(defn guard?
  [ir id]
  (match ir
         {:tag :not :pred {:tag :member :elem (_ :guard #(= % id)) :set _}} true
         {:tag :member :elem (_ :guard #(= % id)) :set _} true
         _ false))

(defn transpose-bitvector
  [bv]
  (let [shape (m/shape bv)]
    (flatten (m/transpose (m/reshape (m/matrix bv) shape)))))

(defn setexpr?
  [expr]
  (let [T (get-type expr)]
    (cond
      (= T "INTEGER") false
      :else true)))

(defn intexpr?
  [expr]
  (= "INTEGER" (get-type expr)))

(defn image
  [bv s]
  (let [elems (comprehend :x (bmember? :x s))
        bools (mapcat elem->bools elems)]
    (filter (fn [ir] (involves? ir bools)) bv)))

(defn find-guards
  [op id]
   (s/select [(s/codewalker #(guard? % id))] op))

(defn get-pred-type [pred]
  (match pred
         {:tag :and :preds ps} (get-pred-type (first ps))
         {:tag :not :pred p} (get-pred-type p)
         {:tag :member :elem _ :set S} S))

(defn get-param-elems [ir id]
  (let [guards (apply band (find-guards ir id))
        Type (get-pred-type guards)]
      (get-sub-type-elems Type)))

(defn get-operation
  [name]
  (first (s/select [(OPERATION name)] (:ir @db))))

(defn combine
  [bindings [id elems]]
  (if (empty? bindings)
    (mapv (fn [elem] [[id elem]]) elems)
    (for [b bindings
          e elems]
      (conj b [id e]))))

(defn ids->bindings [ir ids]
  (reduce combine [] (map (fn [id] [id (get-param-elems ir id)]) ids)))

(defn op->bindings
  [op]
  (let [ids (concat (:args op) (get-non-det-ids op))]
    (ids->bindings op ids)))

(defn get-sets []
  (s/select [(CLAUSE :sets) :values s/ALL] (:ir @db)))
