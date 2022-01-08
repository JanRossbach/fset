(ns hhu.fset.backend.specter-util
  (:require [com.rpl.specter :as s]
            [hhu.fset.backend.util :refer [create-boolname]]
            [lisb.translation.lisb2ir :refer [band]]))

(defn TAG [t] (s/path #(= (:tag %) t)))
(def CLAUSES (s/if-path (s/must :ir) [:ir :clauses] [:machine-clauses]))
(defn CLAUSE [^clojure.lang.Keyword name] (s/path [CLAUSES s/ALL (TAG name)]))
(def VARIABLES (s/path [(CLAUSE :variables) :values]))
(def INVARIANTS (s/path [(CLAUSE :invariants) :values]))
(def PROPERTIES (s/path [(CLAUSE :properties) :values]))
(def CONSTANTS (s/path [(CLAUSE :constants) :values]))
(def SETS (s/path [(CLAUSE :sets) :values s/ALL]))
(defn OPERATION [name] (s/path [(CLAUSE :operations) :values s/ALL #(= (:name %) name)]))

(defn get-vars [ir]
  (s/select [VARIABLES s/ALL] ir))

(defn get-constants [ir]
  (s/select [CONSTANTS s/ALL] ir))

(defn variable?
  [ir id]
  (seq (s/select [VARIABLES s/ALL #(= % id)] ir)))

(defn carrier?
  [ir id]
  (seq (s/select [SETS :id #(= % id)] ir)))

(defn constant?
  [ir id]
  (seq (s/select [CONSTANTS s/ALL #(= % id)] ir)))

(defn replace-param
  [ir [parameter element]]
  (s/setval [(s/walker #(= % parameter))] element ir))

(defn apply-binding
  [body binding]
  (reduce replace-param body binding))

(defn get-props-as-pred
  [ir]
  (let [props (s/select [PROPERTIES s/ALL] ir)]
    (if (seq props)
      (apply band props)
      {:tag :and :preds '()})))

(defn get-invars-as-pred
  [ir]
  (let [invars (s/select [INVARIANTS s/ALL] ir)]
    (if (seq invars)
      (apply band invars)
      {:tag :and :preds '()})))

(defn get-props-and-invars-as-pred
  [ir]
  (let [props (:preds (get-props-as-pred ir))
        invars (:preds (get-invars-as-pred ir))]
    {:tag :and
     :preds (concat props invars)}))

(defn deff-set->enum-set
  [size {:keys [id]}]
  {:tag :enumerated-set
   :id id
   :elems (map (fn [i] (create-boolname (first (name id)) i)) (range 1 (inc size)))})

(defn replace-def-sets-with-enum-sets
  [size ir]
  (s/transform [SETS (TAG :deferred-set)] (partial deff-set->enum-set size) ir))

(defn get-sets [ir]
  (s/select [(CLAUSE :sets) :values s/ALL] ir))

(defn get-operation
  [ir name]
  (first (s/select [(OPERATION name)] ir)))

(defn set-element?
  [ir elem]
  (seq (s/select [SETS :elems s/ALL #(= % elem)] ir)))

(defn simple-tuple?
  [ir elem]
  (and (= (:tag elem) :maplet)
       (set-element? ir (:left elem))
       (set-element? ir (:right elem))))

(defn contains-vars?
  [ir expr]
  (let [vars (set (get-vars ir))]
    (seq (s/select [(s/walker #(contains? vars %))] expr))))
