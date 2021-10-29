(ns fset.core
  (:require
   [potemkin :refer [import-vars]]
   [clojure.core.match :refer [match]]
   [fset.util :as u]
   [clojure.spec.alpha :as spec]
   [fset.specs :refer :all]
   [com.rpl.specter :as s]))


(import-vars [])

(defn validate
  "Debugging helper function using clojure spec."
  [u debug]
  (if debug
    (if (spec/valid? :fset/universe u)
      u
      (do (spec/explain :fset/universe u)
          (throw (ex-info "Something went wrong creating the universe. Spec did not match!"
                          {:universe u}))))
    u))


(defn gen-bool-typedefs
  [elems]
  {:tag :and
   :predicates (map (fn [el] {:tag :member :element el :set :BOOL}) elems)})

(defn typedef? [expr]
  (match expr
    {:tag :member :element (_ :guard keyword?) :set (_ :guard type?)} true
    {:tag :subset :subset (_ :guard keyword?) :set (_ :guard type?)} true
    _ false))

(defn type?
  [expr]
  (match expr
    (_ :guard keyword?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin-set :set (_ :guard type?)} true
    {:tag :fin1-set :set (_ :guard type?)} true
    _ false))


(defn boolencode
  [ir]
  (let [x 1
        ] ;; Setup
    ((fn T [e] ;; Recursion through the machine tree
       (match e
              ;; Logical predicates
         {:tag :and :predicates ps} (list {:tag :and :predicates (mapcat T ps)})
         {:tag :or :predicatescates ps} (list {:tag :or :predicates (mapcat T ps)})
         {:tag :implication :predicates ([A B] :seq)} (list {:tag :implication :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})
         {:tag :equivalence :predicates ([A B] :seq)} (list {:tag :equivalence :predicates (list {:tag :and :predicates (T A)} {:tag :and :predicates (T B)})})
         {:tag :not :predicate p} (map (fn [pp] {:tag :not :predicate pp}) (T p))
         {:tag :for-all} e ;; FIXME
         {:tag :exists} e ;; FIXME

         ;; Equality
         {:tag :equal :left l :right #{}} (map (fn [p] {:tag :not :predicate p}) (T l))
         {:tag :equal :left #{} :right r} (map (fn [p] {:tag :not :predicate p}) (T r))
         {:tag :equal :left l :right r} (map (fn [a b] {:tag :equivalence :predicates (list a b)}) (T l) (T r))
         {:tag :not-equal :left l :right r} (map (fn [p] {:tag :not :predicate p}) (T {:tag :equal :left l :right r}))

         ;; Booleans
         :TRUE :TRUE
         :FALSE :FALSE
         :BOOL #{:TRUE :FALSE}
         {:tag :pred->bool} e;; FIXME

         ;; SETS
         #{} (throw (ex-info "Empty set got matched" {:e e}))
         {:tag :singleton-set} e;; FIXME
         {:tag :enumeration} e;; FIXME
         {:tag :comp-set} e;; FIXME
         {:tag :power-set :set (_ :guard type?)} e
         {:tag :power1-set :set (_ :guard type?)} e
         {:tag :fin-set :set (_ :guard type?)} e
         {:tag :fin1-set :set (_ :guard type?)} e
         {:tag :cardinality} e;; FIXME
         {:tag :cartesion} e;; FIXME
         {:tag :union :sets ([A B] :seq)} (map (fn [a b] {:tag :or :predicates (list a b)}) (T A) (T B))
         {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] {:tag :and :predicates (list a b)}) (T A) (T B))
         {:tag :difference :sets ([A B] :seq)} {:tag :and :predicates (list (T A) {:tag :not :predicate (T B)})}
         {:tag :member, :element el, :set A} (filter #((partial u/involves? el) %) (T A))
         {:tag :subset :subset s :set S} (map (fn [a b] {:tag :implication :predicates (list a b)}) (T s) (T S))
         {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons {:tag :or :predicates (map (fn [a b] {:tag :and :predicates (list {:tag :not :predicate a} b)}) Ts TS)}
                                                                               (map (fn [a b] {:tag :implication :predicates (list a b)}) Ts TS)))
         {:tag :general-union :set-of-sets ss} (apply (partial map (fn [& s] {:tag :or :predicates s})) (map T ss))
         {:tag :general-intersection :set-of-sets ss} (apply (partial map (fn [& s] {:tag :and :predicates s})) (map T ss))

         ;; FIXME
         ;; Numbers
         ;; Relations
         ;; Functions
         ;; Sequences
         ;; Records
         ;; Strings
         ;; Reals
         ;; Trees
         ;; LET and IF-THEN-ELSE

         ;; Statements

         ;; Machine sections
         {:tag :machine :clauses c :name n} {:tag :machine :clauses (map T c) :name n}
         {:tag :sets  :values _} e
         {:tag :constants :values _} e
         {:tag :variables :values v} {:tag :variables :values (mapcat T v)}
         {:tag :properties :values _} e
         {:tag :operations :values v} {:tag :operations :values (mapcat T v)}

         ;; leaf node
         (A :guard type?) (map (fn [x] {:tag :member :element x :set A}) elems)
         _ e))
     ir)))
