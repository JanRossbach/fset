(ns fset.backend
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [ir->ast b->ir]]
   [lisb.translation.lisb2ir :refer [bmember? bcomprehension-set bexists band]])
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

;; HELPERS

(defn interpret-animator-result
  [result]
  (sort (map (fn [e] (if (seq e) (keyword (apply str e)) (keyword e))) result)))

(def get-statespace
  (memoize (fn [ir] (state-space! (ir->ast ir)))))


(defn- get-set-elems
  [set-ir]
  (let [ss (:ss @db)]
    (interpret-animator-result
     (eval-ir-formula
      ss
      (bcomprehension-set [:x] (bmember? :x set-ir))))))

(defn- get-relation-elems
  [rdomain rrange]
  (let [ss (:ss @db)]
    (interpret-animator-result (eval-ir-formula
              ss
              (bcomprehension-set [:x :y] (band (bmember? :x rdomain)
                                                (bmember? :y rrange)))))))

(defn get-type
  [formula]
  (let [ss (:ss @db)
        formula-ast (ir->ast formula)
        ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
    (.getType (.typeCheck ss ee))))

(defn predicate?
  [ir]
  (= "predicate" (get-type ir)))

(defn- get-possible-var-states
  [ss target-vars other-vars predicate]
  (let [res (eval-ir-formula ss
                             (bcomprehension-set target-vars
                                                 (bexists other-vars predicate)))]
    (if (= res :timeout)
      (throw (ex-info "Call to the API timed out." {:target-var target-vars
                                                    :other-vars other-vars
                                                    :predicate predicate}))
      res)))

(defn- typestring->ir
  [type-str]
  (let [query-str (str "MACHINE scheduler\nCONSTANTS x\nPROPERTIES x:" type-str "\nEND")
        query-ir (b->ir query-str)]
    (first (s/select [(CLAUSE :properties) :values s/FIRST :set] query-ir))))

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

(defn- involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn- deff-set->enum-set
  [size {:keys [id]}]
  {:tag :enumerated-set
   :id id
   :elems (map (fn [i] (create-boolname (first (name id)) i)) (range 1 (inc size)))})

(defn- replace-def-sets-with-enum-sets
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
  (let [nir (replace-def-sets-with-enum-sets 2 ir)]
    (reset! db (assoc
                {}
                :max-size 5
                :ir nir
                :ss (get-statespace nir)))))

(defn carrier?
  [id]
  (seq (s/select [SETS :id #(= % id)] (:ir @db))))

(defn create-boolname [& ids]
  (keyword (apply str (map (fn [id] (if (keyword? id) (name id) id)) (flatten ids)))))

(defn set-element?
  [id]
  (seq (s/select [SETS :elems s/ALL #(= % id)] (:ir @db))))

(defn variable?
  [id]
  (seq (s/select [VARIABLES s/ALL #(= % id)] (:ir @db))))

(defn finite-type?
  [expr]
  (if (carrier? expr)
    true
    (let [TS (get-type expr)
          TIR (typestring->ir TS)]
      (match TIR
        {:tag :power-set :set (_ :guard finite-type?)} true
        {:tag :integer-set} false
        {:tag :relation :sets ([A B] :seq)} (and (carrier? A) (carrier? B))
        _ false))))

(defn finite?
  [expr]
  (match expr
    {:tag :interval :from n :to m} (< (- m n) (:max-size @db))
    {:tag :cardinality} true
    _ (finite-type? expr)))


(defn unrollable-var?
  [var-id]
  (and (variable? var-id) (finite? var-id)))

(defn- varid->elems
  [var-id]
  (let [TS (get-type var-id)
        TIR (typestring->ir TS)]
    (match TIR
           {:tag :power-set :set S} (get-set-elems S)
           {:tag :relation :sets ([A B] :seq)} (get-relation-elems A B)
           )))

(defn finite-var?
  [var-id]
  (and (variable? var-id) (finite? var-id)))

(defn unroll-variable
  [var-id]
  (if (finite-var? var-id)
    (map (partial create-boolname var-id) (varid->elems var-id))
    (list var-id)))

(defn get-all-elems-from-elem
  [elem-id]
  (s/select [SETS (fn [m] (some #(= % elem-id) (:elems m))) :elems s/ALL] (:ir @db)))

(defn elem->bools
  [elem]
  (let [TS (get-type elem)
        vars (first (s/select [VARIABLES] (:ir @db)))
        relevant-vars (filter #(= (get-type %) (str "POW(" TS ")")) vars)]
    (map (fn [var-id] (create-boolname var-id elem)) relevant-vars)))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (elem->bools el-id))) formulas))

(defn guard?
  [ir id]
  (match ir
         {:tag :not :pred {:tag :member :elem (_ :guard #(= % id)) :set _}} true
         {:tag :member :elem (_ :guard #(= % id)) :set _} true
         _ false))

(defn bv->shape
  [bv]
  [2 2])

(defn invert-bitvector
  [bv]
  (let [shape (bv->shape bv)]
    (flatten (m/transpose (m/reshape (m/matrix bv) shape)))))

(defn setexpr?
  [expr]
  (let [T (get-type expr)]
    (cond
      (= (:tag expr) :cardinality) true
      (= T "INTEGER") false
      :else true)))

(defn image
  [bv s]
  bv)

(defn find-guards
  [op id]
   (s/select [(s/codewalker #(guard? % id))] op))

(defn get-param-elems [ir id]
  (let [vars (s/select [VARIABLES s/ALL] (:ir @db))
        guards (apply band (find-guards ir id))]
    (interpret-animator-result
     (get-possible-var-states
      (:ss @db)
      id
      vars
      (band guards (get-props-and-invars-as-pred))))))

(defn get-operation
  [name]
  (first (s/select [(OPERATION name)] (:ir @db))))

(defn combine
  [op bindings id]
  (let [elems (get-param-elems op id)]
    (if (empty? bindings)
      (mapv (fn [elem] [[id elem]]) elems)
      (for [b bindings
            e elems]
        (conj b [id e])))))

(defn op->bindings
  [op]
  (let [ids (concat (:args op) (get-non-det-ids op))]
    (reduce (partial combine op) [] ids)))

(defn get-sets []
  (s/select [(CLAUSE :sets) :values s/ALL] (:ir @db)))
