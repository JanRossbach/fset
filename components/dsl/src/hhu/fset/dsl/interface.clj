(ns hhu.fset.dsl.interface
  (:require
   [lisb.translation.lisb2ir :refer [b= bpred->bool band bor bnot bequivalence bimplication bmember? bif-expr bassign b+ b<= b>= bunion]]
   [clojure.core.matrix :as m]))

(def TRUE
  (b= true true))

(def FALSE
  (b= true false))

(defn BOOL
  [p]
  (bpred->bool p))

(defn AND [& ps]
  (apply band ps))

(defn OR [& ps]
  (apply bor ps))

(defn NOT [p]
  (bnot p))

(defn <=> [& ps]
  (apply bequivalence ps))

(defn => [& ps]
  (apply bimplication ps))

(defn EQUALS [l r]
  (b= l r))

(defn =TRUE [p]
  (b= p true))

(defn IN [el s]
  (bmember? el s))

(defn BOOLDEF [el]
  (bmember? el {:tag :bool-set}))

(defn BOOLDEFS [els]
  (if (seq els)
    (apply AND (map BOOLDEF els))
    {}))

(defn IF [pred then else]
  (bif-expr pred then else))

(defn ASSIGN [id val]
  (bassign id val))

(defn MACHINE [name clauses]
  {:tag :machine
   :name name
   :machine-clauses clauses
   :args []})

(defn CARDINALITY [PRED elems]
  (apply b+ (map (fn [e] (IF (PRED e) 1 0)) elems)))

(defn FUN [elem-matrix]
  (apply band (map (fn [row] (b<= (CARDINALITY (fn [e] (=TRUE e)) row) 1)) elem-matrix)))

(defn TOTAL-FUN [elem-matrix]
  (apply band (map (fn [row] (b= (CARDINALITY (fn [e] (=TRUE e)) row) 1)) elem-matrix)))

(defn SURJECTIVE [elem-matrix]
  (apply band (map (fn [row] (b>= (CARDINALITY (fn [e] (=TRUE e)) row) 1)) elem-matrix)))

(defn INJECTIVE [elem-matrix]
  (FUN (m/transpose elem-matrix)))

(defn BIJECTIVE [elem-matrix]
  (TOTAL-FUN (m/transpose elem-matrix)))

(defn bv->setexpr [bv]
  (apply bunion
   (map (fn [{:keys [name elem]}] (IF (=TRUE name) #{elem} #{})) bv)))
