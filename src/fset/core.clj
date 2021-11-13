(ns fset.core
  (:require
   [clojure.core.match :refer [match]]
   [fset.dsl :refer [AND OR =TRUE <=> NOT TRUE FALSE EQUAL => BOOL BOOLDEFS IF ASSIGN]]
   [fset.util :as util]
   [com.rpl.specter :as s]
   [fset.backend :as b]))

(def m 5) ;; MAX-SET-SIZE

(defn set->bitvector
  [Set]
  ((fn T [e]
     (match e
       #{} (repeat m FALSE)
       (singleton-set :guard #(and (set? %) (= 1 (count %)))) (let [x (first singleton-set)] (map (fn [e] (if (= e x) TRUE FALSE)) (b/get-all-elems-from-elem x)))
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} (map (fn [a b] (AND a (NOT b))) (T A) (T B))
       {:tag :general-union :set-of-sets ss} (apply map OR (map T ss))
       {:tag :general-intersection :set-of-sets ss} (apply map AND (map T ss))
       {:tag :card :set s} (list {:tag :plus :numbers (map (fn [p] (IF (=TRUE (BOOL p)) 1 0)) (T s))})
       {:tag :minus :numbers ns} (T {:tag :difference :sets ns})
       (n :guard number?) (list n)
       (variable :guard b/variable?) (map =TRUE (b/unroll-variable variable))
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
       {:tag :subset :subset (s :guard b/unrollable-var?) :set (_ :guard b/type?)} (list (BOOLDEFS (b/unroll-variable s)))
       {:tag :subset :subset s :set S} (map (fn [a b] (=> a b)) (T s) (T S))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                             (map (fn [a b] (=> a b)) Ts TS)))
         ;; logical operators
       {:tag :and :predicates ps} (list (apply AND (mapcat T ps)))
       {:tag :or :predicates ps} (list (apply OR (mapcat T ps)))
       {:tag :not :predicate p} (map NOT (T p))
       {:tag :equivalence :predicates ([A B] :seq)} (list (<=> (apply AND (T A)) (apply AND (T B))))
       {:tag :implication :predicates ([A B] :seq)} (list (=> (apply AND (T A)) (apply AND (T B))))

       ;; Member
       {:tag :member :element (_ :guard b/set-element?) :set (_ :guard b/type?)} '()
       {:tag :member :element (el-id :guard b/set-element?) :set v} (b/pick-bool-var (set->bitvector v) el-id)
       {:tag :member :element (v :guard b/unrollable-var?) :set (_ :guard b/type?)} (list (BOOLDEFS (b/unroll-variable v)))

       ;; Numbers
       {:tag :less-eq :numbers ns} (list {:tag :less-eq :numbers (mapcat T ns)})

       (SET :guard b/enumerable?) (set->bitvector SET)
       _ e))
   pred))

(defn unroll-sub
  [sub]
  ((fn T [e]
     (match e
       {:tag :skip} (list {:tag :skip})
       {:tag :parallel-substitution :substitutions substitutions} {:tag :parallel-substitution :substitutions (map T substitutions)}
       {:tag :assign :identifiers identifiers :values values} {:tag :parallel-substitution :substitutions (map (fn [v p] (ASSIGN v (BOOL p))) (mapcat b/unroll-variable identifiers) (mapcat set->bitvector values))}
       {:tag :if-sub :condition condition :then then :else else} {:tag :if-sub :condition (apply AND (unroll-predicate condition)) :then (T then) :else (T else)}
       {:tag :select :clauses ([A B & _] :seq)} {:tag :select :clauses (list (apply AND (unroll-predicate A)) (T B))}
       {:tag :any :identifiers _ :where _ :then then} (T then)
       _ e))
   sub))

(defn replace-param
  [body [parameter element]]
  (s/setval [(s/walker #(= % parameter))] element body))

(defn apply-binding
  [body binding]
  (reduce replace-param body binding))

(defn non-det-clause?
  [pattern]
  (match pattern
    {:tag :any} true
    _ false))

(defn get-non-det-clauses
  [op]
  (s/select (s/walker non-det-clause?) op))

(defn get-non-det-guards
  [op]
  (let [non-det-clauses (get-non-det-clauses op)]
    (map (fn [clause]
           (match clause
             {:tag :any :where w} w))
         non-det-clauses)))

(defn add-guards
  [op guards]
  (assoc op :body {:tag :select :clauses (list (apply AND guards) (:body op))}))

(defn lift-guards
  [op]
  (let [guards (get-non-det-guards op)]
    (if (empty? guards)
      op
      (add-guards op guards))))

(defn new-op
  [old-op binding]
  {:tag :operation
   :name (apply b/create-boolname (:name old-op) (map second binding))
   :return []
   :parameters []
   :body (-> old-op
             lift-guards
             :body
             (apply-binding binding)
             unroll-sub)})

(defn unroll-operation
  [op]
  (let [bindings (b/calc-op-combinations (:name op))]
    (if (seq bindings)
      (map (partial new-op op) bindings)
      (list (assoc op :body (unroll-sub (:body op)))))))

(defn unroll-clause
  [c]
  (match c
         {:tag :variables :values v} {:tag :variables :values (mapcat b/unroll-variable v)}
         {:tag :invariants :values v} {:tag :invariants :values (mapcat unroll-predicate v)}
         {:tag :init :values v} {:tag :init :values (map unroll-sub v)}
         {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
         _ c))

(defn simplify-formula
  [formula]
  (match formula
    {:tag :not :predicate {:tag :not :predicate p}} p
    {:tag :pred->bool :predicate {:tag :equal :left :TRUE :right :FALSE}} :FALSE
    {:tag :pred->bool :predicate {:tag :equal :left :TRUE :right :TRUE}} :TRUE
    {:tag :not :predicate {:tag :equal :left :TRUE :right :TRUE}} FALSE
    {:tag :not :predicate {:tag :equal :left :TRUE :right :FALSE}} TRUE
    {:tag :or :predicates (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :predicates (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :predicates (_ :guard (fn [ps] (some #(= % FALSE) ps)))} FALSE
    {:tag :or :predicates (_ :guard (fn [ps] (some #(= % TRUE) ps)))} TRUE
    {:tag :and :predicates (ps :guard (fn [ps] (some #(= % TRUE) ps)))} {:tag :and :predicates (filter #(not= % TRUE) ps)}
    {:tag :or :predicates (ps :guard (fn [ps] (some #(= % FALSE) ps)))} {:tag :or :predicates (filter #(not= % FALSE) ps)}
    {:tag :equal :left {:tag :pred->bool :predicate p} :right :TRUE} p
    {:tag :assign :identifiers ([a] :seq) :values ([{:tag :pred->bool :predicate {:tag :equal :left b :right :TRUE}}] :seq)} (if (= a b) s/NONE nil)
    {:tag :pred->bool :predicate {:tag :plus :numbers n}} {:tag :plus :numbers n}
    {:tag :parallel-substitution :substitutions (substitutions :guard #(= (count %) 1))} (first substitutions)
    {:tag :parallel-substitution :substitutions (_ :guard empty?)} s/NONE
    {:tag :select :clauses ([outer-guard {:tag :select :clauses ([inner-guard & r] :seq)}] :seq)} {:tag :select :clauses (cons (AND outer-guard inner-guard) r)}
    _ nil))


(defn simplify-ir
  [ir]
  (s/transform [(s/walker simplify-formula)] simplify-formula ir))

(defn simplify-all
  [ir]
  (loop [IR ir]
    (let [next-ir (simplify-ir IR)]
      (if (= IR next-ir)
        next-ir
        (recur next-ir)))))

(defn boolencode
  [ir]
  (simplify-all {:tag :machine
                 :name (:name ir)
                 :clauses (map unroll-clause (:clauses ir))}))
