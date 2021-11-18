(ns fset.backend
  (:require
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.prob.animator :refer [state-space!]]
   [lisb.translation.util :refer [lisb->ir ir->ast b->ir]]
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

(declare get-statespace get-max-size create-boolname)

(def db (atom {}))

;; HELPERS

(defn interpret-animator-result
  [result]
  (sort (map keyword result)))

(def get-statespace
  (memoize (fn [ir] (state-space! (ir->ast ir)))))

(defn- get-set-elems
  [set-id]
  (let [ss (:ss @db)]
    (interpret-animator-result
     (eval-ir-formula ss
                      (lisb->ir `(bcomprehension-set [:x] (bmember? :x ~set-id)))))))

(defn get-type
  [formula]
  (let [ss (:ss @db)
        formula-ast (ir->ast formula)
        ee (ClassicalB. formula-ast FormulaExpand/TRUNCATE "")]
    (.getType (.typeCheck ss ee))))

(defn- get-possible-var-states
  [ss target-vars other-vars predicate]
  (let [res (eval-ir-formula ss {:tag :comprehension-set
                                 :ids target-vars
                                 :pred {:tag :exists :ids other-vars :pred predicate}})]
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
      (b= :TRUE :TRUE))))

(defn get-invars-as-pred
  []
  (let [invars (s/select [INVARIANTS s/ALL] (:ir @db))]
    (if (seq invars)
      (apply band invars)
      (b= :TRUE :TRUE))))

(defn unroll-set-expr
  [set-expr]
  (let [{:keys [ir ss]} @db
        vars (s/select [VARIABLES s/ALL] ir)
        constants (s/select [CONSTANTS s/ALL] ir)
        invariant (get-invars-as-pred)
        properties (get-props-as-pred)]
    (eval-ir-formula
     ss
     (bcomprehension-set
      [:x]
      (bexists
       (concat vars constants)
       (band
        (bmember? :x set-expr)
        invariant
        properties))))))


(defn- involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn- deff-set->enum-set
  [size {:keys [id]}]
  {:tag :enumerated-set
   :id id
   :elems (map (fn [i] (create-boolname id i)) (range 1 (inc size)))})

(defn- replace-def-sets-with-enum-sets
  [size ir]
  (s/transform [SETS (TAG :deferred-set)] (partial deff-set->enum-set size) ir))

;; Public API

(defn get-max-size [] (:max-size @db))

(defn setup-backend ;; Call this before testing the backend isolated without an actual translation
  [ir]
  (let [nir (replace-def-sets-with-enum-sets 3 ir)]
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

(defn finite?
  [expr]
  (get-type expr))

(defn unrollable-var?
  [var-id]
  (and (variable? var-id) (finite? var-id)))

(defn- varid->elems
  [var-id]
  (let [TS (get-type var-id)
        TIR (typestring->ir TS)]
    (match TIR
           {:tag :power-set :set S} (get-set-elems S))))

(defn unroll-variable
  [var-id]
  (if (and (variable? var-id) (finite? var-id))
    (map (partial create-boolname var-id) (varid->elems var-id))
    (list var-id)))

(defn get-all-elems-from-elem
  [elem-id]
  (s/select [SETS (fn [m] (some #(= % elem-id) (:elems m))) :elems s/ALL] (:ir @db)))

(defn elem->bools
  [elem]
  (let [TS (get-type elem)
        TIR (typestring->ir TS)
        vars (first (s/select [VARIABLES] (:ir @db)))
        relevant-vars (filter #(= (get-type %) (str "POW(" TS ")")) vars)]
    (map (fn [var-id] (create-boolname var-id elem)) relevant-vars)))

;; FIXME
(defn op->bindings
  [op]
  (match op
    (_ :guard #(= (:name %) :nr_ready)) []
    (_ :guard #(= (:name %) :new)) [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
    (_ :guard #(= (:name %) :ready)) [[[:rr :PID1]] [[:rr :PID2]] [[:rr :PID3]]]
    (_ :guard #(= (:name %) :del)) [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]
    (_ :guard #(= (:name %) :swap)) [[[:pp :PID1]] [[:pp :PID2]] [[:pp :PID3]]]))

(defn pick-bool-var
  [formulas el-id]
  (filter (fn [formula] (involves? formula (elem->bools el-id))) formulas))

(defn get-op-combinations [op-id]
    (op->bindings {:name op-id}))
