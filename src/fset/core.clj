(ns fset.core
  (:require
   [potemkin :refer [import-vars]]
   [clojure.core.match :refer [match]]
   [fset.dsl :refer [AND OR =TRUE =FALSE <=> NOT IN TRUE FALSE EQUAL => BOOL BOOLDEFS IF]]
   [fset.util :as u]
   [clojure.spec.alpha :as spec]
   [fset.specs :refer :all]
   [com.rpl.specter :as s]
   [fset.backend :as b]))

(import-vars [])

;; Only used to store some global information to avoid calling the backend multiple times or passing around the values in every function.
;; The value is not to be altered after initialization.
(def app-db (atom {}))

(defn set-element?
  [id]
  (let [set-elems (:set-elems @app-db)]
    (some #(= % id) set-elems)))

(defn involves?
  [ir ids]
  (seq (s/select [(s/walker (fn [w] (some #(= % w) ids)))] ir)))

(defn carrier?
  [id]
  (let [sets (:sets @app-db)]
    (some #(= % id) sets)))

(defn type?
  [expr]
  (match expr
    (_ :guard carrier?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin-set :set (_ :guard type?)} true
    {:tag :fin1-set :set (_ :guard type?)} true
    _ false))

(defn unrollable-pred?
  [_]
  true)

(defn unrollable-op?
  [op]
  (some #(= % (:name op)) (:unroll-ops @app-db)))

(defn unrollable-var?
  [_]
  true)

(defn enum-set?
  [s]
  true)

(defn variable?
  [id]
  true)

(defn enumerable?
  [s]
  true)

(defn typedef? [expr]
  (match expr
    {:tag :member :element (_ :guard keyword?) :set (_ :guard type?)} true
    {:tag :subset :subset (_ :guard keyword?) :set (_ :guard type?)} true
    _ false))

(defn boolname
  [var-id el-id]
  (if (and (keyword? var-id) (keyword? el-id))
    (keyword (str (name var-id) (name el-id)))
    (throw (ex-info "One of the ID's given to boolname was not a keyword.
Take care, that all patterns are handled in set->bitvector" {:var-id var-id
                                                             :el-id el-id}))))
(defn replace-param
  [body parameters id]
  (s/setval [(s/walker (fn [w] (some #(= % w) parameters)))] id body))

(defn get-elems-by-id
  [id]
  (let [db @app-db
        elem-map (:elem-map db)]
    (get elem-map id)))

(defn unroll-variable
  [var-id]
  (if (unrollable-var? var-id)
    (map (partial boolname var-id) (get-elems-by-id var-id))
    (list var-id)))

(defn set->bitvector
  [elems Set]
  ((fn T [e]
     (match e
       #{} (repeat (count elems) FALSE)
       (singleton-set :guard #(and (set? %) (= 1 (count %)))) (let [x (first singleton-set)] (map (fn [e] (if (= e x) TRUE FALSE)) elems))
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} (map (fn [a b] (AND a (NOT b))) (T A) (T B))
       {:tag :general-union :set-of-sets ss} (apply map OR (map T ss))
       {:tag :general-intersection :set-of-sets ss} (apply map AND (map T ss))
       {:tag :card :set s} {:tag :plus :numbers (map (fn [p] (IF (=TRUE (BOOL p)) 1 0)) (T s))}
       {:tag :minus :numbers ns} (T {:tag :difference :sets ns})
       (n :guard number?) n
       (variable :guard variable?) (->> elems (map (partial boolname variable)) (map =TRUE))
       _ e))
   Set))

(defn unroll-predicate
  "Takes a B predicate of type POW(S), that is not a type definition, and a collection of elements, and returns
  a collection of predicates that represent the unrolled expression."
  [pred]
  ((fn T [e]
     (match e
         ;; equality
       {:tag :equal :left l :right #{}} (list (apply AND (map NOT (T l))))
       {:tag :equal :left #{} :right r} (list (apply AND (map NOT (T r))))
       {:tag :equal :left l :right r} (list (apply AND (map <=> (T l) (T r))))
       {:tag :not-equal :left l :right r} (list (apply NOT (T (EQUAL l r))))
       {:tag :subset :subset (s :guard unrollable-var?) :set (_ :guard type?)} (list (BOOLDEFS (unroll-variable s)))
       {:tag :subset :subset s :set S} (map (fn [a b] (=> a b)) (T s) (T S))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                             (map (fn [a b] (=> a b)) Ts TS)))
         ;; logical operators
       {:tag :and :predicates ps} (list (apply AND (mapcat T ps)))
       {:tag :or :predicates ps} (list (apply OR (mapcat T ps)))
       {:tag :not :predicate p} (map NOT (T p))
       {:tag :equivalence :predicates ([A B] :seq)} (list (<=> (apply AND (T A)) (apply AND (T B))))
       {:tag :implication :predicates ([A B] :seq)} (list (=> (apply AND (T A)) (apply AND (T B))))
       {:tag :member :element (_ :guard set-element?) :set (_ :guard type?)} '()
       {:tag :member :element (el-id :guard set-element?) :set (v :guard unrollable-var?)} (list (=TRUE (apply BOOL (set->bitvector (list el-id) v))))
       {:tag :member :element (v :guard unrollable-var?) :set (_ :guard type?)} (list (BOOLDEFS (unroll-variable v)))
       {:tag :less-eq :numbers ns} (list {:tag :less-eq :numbers (map T ns)})
       (SET :guard enumerable?) (set->bitvector '(:PID1 :PID2 :PID3) SET)))
   pred))

(list (apply =TRUE (set->bitvector '(:PID1) {:tag :union :sets '(:active :waiting)})))

(defn unroll-init-substitution
  [elems sub]
  ((fn T [e]
     (match e
       {:tag :skip} {:tag :skip}
       {:tag :parallel-substitution :substitutions substitutions} {:tag :parallel-substitution :substitutions (map T substitutions)}
       {:tag :assign :identifiers identifiers :values values} {:tag :assign :identifiers (mapcat unroll-variable identifiers) :values (map BOOL (mapcat (partial set->bitvector elems) values))}
       {:tag :if-sub :condition condition :then then :else else} {:tag :if-sub :condition (apply AND (unroll-predicate condition)) :then (T then) :else (T else)}
       {:tag :select :clauses ([A B & _] :seq)} {:tag :select :clauses (list (apply AND (unroll-predicate A)) (T B))}
       _ e))
   sub))

(defn unroll-init
  [& values]
  (let [vars (:unroll-vars @app-db)]
    (map (fn [v]
           (if (involves? v vars)
             (unroll-init-substitution '(:PID1 :PID2 :PID3) v)
             v))
         values)))


(defn unroll-op-substitution
  [elems elem-id body]
  ((fn T [e]
     (match e
       {:tag :skip} {:tag :skip}
       {:tag :parallel-substitution :substitutions substitutions} {:tag :parallel-substitution :substitutions (map T substitutions)}
       {:tag :assign :identifiers identifiers :values values} {:tag :assign :identifiers (map (fn [var-id] (boolname var-id elem-id)) identifiers) :values (map BOOL (mapcat (partial set->bitvector (list elem-id)) values))}
       {:tag :select :clauses ([A B & _] :seq)} {:tag :select :clauses (list (apply AND (unroll-predicate A)) (T B))}
       {:tag :if-sub :condition condition :then then :else else} {:tag :if-sub :condition (apply AND (unroll-predicate condition)) :then (T then) :else (T else)}
       {:tag :any :identifiers identifiers :where where :then then} {:tag :select :clauses (list (apply AND (unroll-predicate (replace-param where identifiers elem-id))) (T (replace-param then identifiers elem-id)))}
       _ e))
   body))

(defn new-op
  [old-op elems elem-id]
  {:tag :operation
   :name (boolname (:name old-op) elem-id)
   :return []
   :parameters []
   :body (unroll-op-substitution elems elem-id (replace-param (:body old-op) (:parameters old-op) elem-id))})

(defn unroll-operation
  [op]
  (let [elems '(:PID1 :PID2 :PID3)]
    (if (unrollable-op? op)
      (map (partial new-op op elems) elems)
      (list (assoc op :body (unroll-init-substitution elems (:body op)))))))

(defn unroll-clause
  [c]
  (match c
         {:tag :variables :values v} {:tag :variables :values (mapcat unroll-variable v)}
         {:tag :invariants :values v} {:tag :invariants :values (mapcat unroll-predicate v)}
         {:tag :init :values v} {:tag :init :values (mapcat unroll-init v)}
         {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
         _ c))

(defn boolencode
  [ir]
  (b/init-db ir app-db)
  {:tag :machine
   :name (:name ir)
   :clauses (map unroll-clause (:clauses ir))})
