(ns fset.core
  (:require
   [potemkin :refer [import-vars]]
   [clojure.core.match :refer [match]]
   [fset.dsl :refer [AND OR =TRUE =FALSE <=> NOT IN TRUE FALSE EQUAL => BOOL BOOLDEFS]]
   [fset.util :as u]
   [clojure.spec.alpha :as spec]
   [fset.specs :refer :all]
   [com.rpl.specter :as s]
   [fset.backend :as b]))

(import-vars [])

;; Only used to store some global information to avoid calling the backend multiple times or passing around the values in every function.
;; The value is not to be altered after initialization.
(defonce app-db (atom {}))

(defn init-db
  [ir]
  (let [ss (b/get-statespace ir)
        vars '(:active :ready :waiting)
        unroll-vars '(:active :ready :waiting)
        elem-map {"POW(PID)" '(:PID1 :PID2 :PID3)}]
    (reset!
     app-db
     {:ss ss
      :vars vars
      :unroll-vars unroll-vars
      :elem-map elem-map})))

(defn type?
  [expr]
  (match expr
    (_ :guard keyword?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin-set :set (_ :guard type?)} true
    {:tag :fin1-set :set (_ :guard type?)} true
    _ false))

(defn variable?
  [id]
  true)

(defn typedef? [expr]
  (match expr
    {:tag :member :element (_ :guard keyword?) :set (_ :guard type?)} true
    {:tag :subset :subset (_ :guard keyword?) :set (_ :guard type?)} true
    _ false))

(defn unrollable? [id]
  true)

(defn boolname
  [var-id el-id]
  (keyword (str (name var-id) (name el-id))))

(defn get-elems-by-id
  [id]
  (let [db @app-db
        elem-map (:elem-map db)]
    (get id elem-map)))

(defn get-expr-elems
  [expr]
  (let [{:keys [ss elem-map]} @app-db
        t (b/get-type ss expr)]
    (get t elem-map)))

(defn unroll-set-expression
  "Takes a expression of the set type and a seq of elems and unrolls them into a seq of B predicates in IR form.
  The returned seq will always have the same cardinality as the elems seq and each formula will correspond to exactly that element."
  [expr]
  (let [elems (get-expr-elems expr)]
    ((fn T [e]
       (match e
         #{} (repeat FALSE (count elems))
         (S :guard #(and (set? %) (= 1 (count %)))) (let [x (first S)] (map (fn [el] (if (= el x) TRUE FALSE)) elems)) ;; Singleton Set
         (S :guard set?) (map (fn [el] (if (some #(= el %) S) TRUE FALSE)) elems)
         {:tag :set-enum :elements els} (T (set els))
         {:tag :comp-set} e;; FIXME
         {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
         {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
         {:tag :difference :sets ([A B] :seq)} (AND (T A) (NOT  (T B)))
         {:tag :general-union :set-of-sets ss} (apply (partial map OR) (map T ss))
         {:tag :general-intersection :set-of-sets ss} (apply (partial map AND) (map T ss))
         (ID :guard #(and (variable? %) (unrollable? %))) (map (fn [el] (=TRUE (boolname ID el))) elems)
         (ID :guard type?) (map (partial IN ID) elems)))
     expr)))

(defn unroll-predicate
  "Takes a B predicate of type POW(S), that is not a type definition, and a collection of elements, and returns
  a collection of predicates that represent the unrolled expression."
  [pred]
  ((fn T [e]
     (match e
         ;; equality
       {:tag :equal :left l :right #{}} (map NOT (T l))
       {:tag :equal :left #{} :right r} (map NOT (T r))
       {:tag :equal :left l :right r} (map <=> (T l) (T r))
       {:tag :not-equal :left l :right r} (map NOT (T (EQUAL l r)))
       {:tag :subset :subset s :set S} (map (fn [a b] (=> a b)) (T s) (T S))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                             (map (fn [a b] (=> a b)) Ts TS)))
         ;; logical operators
       {:tag :and :predicates ps} (list (apply AND (mapcat T ps)))
       {:tag :or :predicates ps} (list (apply OR (mapcat T ps)))
       {:tag :not :predicate p} (map NOT (T p))
       {:tag :equivalence :predicates ([A B] :seq)} (list (<=> (apply AND (T A)) (apply AND (T B))))
       {:tag :implication :predicates ([A B] :seq)} (list (=> (apply AND (T A)) (apply AND (T B))))
       {:tag :member :element x :set s} (T {:tag :subset :subset x :set (:set s)})
       expr (unroll-set-expression expr)))
   pred))

(defn unroll-substitution
  [sub]
  (let [elems '(:PID1 :PID2 :PID3)]
    ((fn T [e]
       (match e
         {:tag :skip} {:tag :skip}
         {:tag :assign :identifiers identifiers :values values} {:tag :assign :identifiers (map :left (mapcat T identifiers)) :values (map BOOL (mapcat T values))}
         _ e))
     sub)))

(defn unrollable-pred?
  [_]
  true)

(defn unrollable-op?
  [_]
  true)

(defn new-op
  [old-op elem-id]
  {:name :Test
   :return []
   :parameters []
   :body (unroll-substitution (:body old-op))})

(defn unroll-operation
  [op]
  (let [elems '(:PID1 :PID2 :PID3)]
    (map (partial new-op op) elems)))

(defn boolencode
  [ir]
  (init-db ir)
  ((fn T [e] ;; Recursion through the machine tree
     (match e
              ;; Machine sections
            {:tag :machine :clauses c :name n} {:tag :machine :clauses (map T c) :name n}
            {:tag :sets  :values _} e
            {:tag :constants :values _} e
            {:tag :variables :values v} {:tag :variables :values v}
            {:tag :invariants :values v} {:tag :invariants :values (mapcat unroll-predicate (filter unrollable-pred? v))}
            {:tag :properties :values _} e
            {:tag :init :values _} e
            {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation (filter unrollable-op? v))}))
     ir))
