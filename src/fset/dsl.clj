(ns fset.dsl
  (:require
   [lisb.translation.lisb2ir :refer [b= bpred->bool band bor bnot b<=> b=> bsubset? bmember? bif-expr bassign]]))

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
  (apply b<=> ps))

(defn => [& ps]
  (apply b=> ps))

(defn EQUAL [l r]
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
  (apply AND (map BOOLDEF els)))

(defn IF [pred then else]
  (bif-expr pred then else))

(defn ASSIGN [id val]
  (bassign id val))
