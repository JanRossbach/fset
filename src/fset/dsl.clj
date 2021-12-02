(ns fset.dsl
  (:require
   [lisb.translation.lisb2ir :refer [b= bpred->bool band bor bnot bequivalence bimplication bsubset? bmember? bif-expr bassign b+ b<= b>=]]
   [clojure.core.matrix :as m]))

(def TRUE
  (b= :TRUE :TRUE))

(def FALSE
  (b= :TRUE :FALSE))

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

(defn SUBSET [sub set]
  (bsubset? sub set))

(defn =FALSE [p]
  (b= p :FALSE))

(defn =TRUE [p]
  (b= p :TRUE))

(defn IN [el s]
  (bmember? el s))

(defn BOOLDEF [el]
  (bmember? el :BOOL))

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
   :clauses clauses})

(defn CARDINALITY [PRED elems]
  (apply b+ (map (fn [e] (IF (PRED e) 1 0)) elems)))

(def mat [[:yA1B1 :yA1B2] [:yA2B1 :yA2B2]])

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
