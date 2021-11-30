(ns fset.dsl
  (:require
   [lisb.translation.lisb2ir :refer [b= bpred->bool band bor bnot bequivalence bimplication bsubset? bmember? bif-expr bassign bmachine]]))

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

(defn IN [s el]
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
