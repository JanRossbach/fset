(ns fset.dsl)

(def TRUE
  {:tag :equal
   :left :TRUE
   :right :TRUE})

(def FALSE
  {:tag :equal
   :left :FALSE
   :right :TRUE})

(defn BOOL
  [p]
  {:tag :pred->bool
   :predicate p})

(defn AND [& ps]
  {:tag :and
   :predicates ps})

(defn OR [& ps]
  {:tag :or
   :predicates ps})

(defn NOT [p]
  {:tag :not
   :predicate p})

(defn <=> [& ps]
  {:tag :equivalence
   :predicates ps})

(defn => [& ps]
  {:tag :implication
   :predicates ps})

(defn EQUAL [l r]
  {:tag :equal
   :left l
   :right r})

(defn SUBSET [sub set]
  {:tag :subset
   :subset sub
   :set set})

(defn =FALSE [p]
  {:tag :equal
   :left p
   :right :FALSE})

(defn =TRUE [p]
  {:tag :equal
   :left p
   :right :TRUE})

(defn IN [s el]
  {:tag :member
   :element el
   :set s})

(defn BOOLDEF [el]
  {:tag :member :element el :set :BOOL})

(defn BOOLDEFS [els]
  (apply AND (map BOOLDEF els)))
