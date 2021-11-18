(ns fset.core
  (:require
   [fset.dsl :refer [AND OR =TRUE <=> NOT TRUE FALSE EQUALS => BOOL BOOLDEFS IF ASSIGN]]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [fset.backend :as b]))



(defn- type?
  [expr]
  (match expr
    (_ :guard b/carrier?) true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin :set (_ :guard type?)} true
    {:tag :fin1 :set (_ :guard type?)} true
    _ false))

(defn- set->bitvector
  [Set]
  ((fn T [e]
     (match e
       #{} (repeat (b/get-max-size) FALSE)
       (singleton-set :guard #(and (set? %) (= 1 (count %)))) (let [x (first singleton-set)] (map (fn [e] (if (= e x) TRUE FALSE)) (b/get-all-elems-from-elem x)))
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} (map (fn [a b] (AND a (NOT b))) (T A) (T B))
       {:tag :unite-sets :set-of-sets ss} (apply map OR (map T ss))
       {:tag :intersect-sets :set-of-sets ss} (apply map AND (map T ss))
       {:tag :cardinality :set s} (list {:tag :add :nums (map (fn [p] (IF (=TRUE (BOOL p)) 1 0)) (T s))})
       {:tag :sub :nums ns} (T {:tag :difference :sets ns})
       (n :guard number?) (list n)
       (variable :guard b/variable?) (map =TRUE (b/unroll-variable variable))
       _ e))
   Set))


(defn- unroll-predicate
  [pred]
  ((fn T [e]
     (match e
         ;; equalsity
       {:tag :equals :left l :right #{}} (list (apply AND (map NOT (T l))))
       {:tag :equals :left #{} :right r} (list (apply AND (map NOT (T r))))
       {:tag :equals :left l :right r} (list (apply AND (map <=> (T l) (T r))))
       {:tag :not-equals :left l :right r} (list (apply NOT (T (EQUALS l r))))
       {:tag :subset :sets ([(s :guard b/unrollable-var?) (_ :guard type?)] :seq)} (list (BOOLDEFS (b/unroll-variable s)))
       {:tag :subset :sets ([s S] :seq)} (map (fn [a b] (=> a b)) (T s) (T S))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                             (map (fn [a b] (=> a b)) Ts TS)))
         ;; logical operators
       {:tag :and :preds ps} (list (apply AND (mapcat T ps)))
       {:tag :or :preds ps} (list (apply OR (mapcat T ps)))
       {:tag :not :pred p} (map NOT (T p))
       {:tag :equivalence :preds ([A B] :seq)} (list (<=> (apply AND (T A)) (apply AND (T B))))
       {:tag :implication :preds ([A B] :seq)} (list (=> (apply AND (T A)) (apply AND (T B))))

       ;; Member
       {:tag :member :elem (_ :guard b/set-element?) :set (_ :guard type?)} '()
       {:tag :member :elem (el-id :guard b/set-element?) :set v} (b/pick-bool-var (set->bitvector v) el-id)
       {:tag :member :elem (v :guard b/unrollable-var?) :set (_ :guard type?)} (list (BOOLDEFS (b/unroll-variable v)))

       ;; Numbers
       {:tag :less-equals :nums ns} (list {:tag :less-equals :nums (mapcat T ns)})

       (SET :guard b/finite?) (set->bitvector SET)
       _ e))
   pred))

(defn- unroll-sub
  [sub]
  ((fn T [e]
     (match e
       {:tag :skip} (list {:tag :skip})
       {:tag :parallel-sub :subs substitutions} {:tag :parallel-sub :subs (map T substitutions)}
       {:tag :assignment :id-vals ([id value] :seq)} {:tag :parallel-sub :subs (map (fn [v p] (ASSIGN v (BOOL p))) (b/unroll-variable id) (set->bitvector value))}
       {:tag :if-sub :cond condition :then then :else else} {:tag :if-sub :cond (apply AND (unroll-predicate condition)) :then (T then) :else (T else)}
       {:tag :select :clauses ([A B & _] :seq)} {:tag :select :clauses (list (apply AND (unroll-predicate A)) (T B))}
       {:tag :any :ids _ :pred _ :subs then} {:tag :parallel-sub :subs (map T then)}
       _ e))
   sub))

(defn- replace-param
  [body [parameter element]]
  (s/setval [(s/walker #(= % parameter))] element body))

(defn- apply-binding
  [body binding]
  (reduce replace-param body binding))

(defn- non-det-clause?
  [pattern]
  (match pattern
    {:tag :any} true
    _ false))

(defn- get-non-det-clauses
  [op]
  (s/select (s/walker non-det-clause?) op))

(defn- get-non-det-guards
  [op]
  (let [non-det-clauses (get-non-det-clauses op)]
    (map (fn [clause]
           (match clause
             {:tag :any :pred w} w))
         non-det-clauses)))

(defn- add-guards
  [op guards]
  (assoc op :body {:tag :select :clauses (list (apply AND guards) (:body op))}))

(defn- lift-guards
  [op]
  (let [guards (get-non-det-guards op)]
    (if (empty? guards)
      op
      (add-guards op guards))))

(defn- new-op
  [old-op binding]
  {:tag :op
   :name (apply b/create-boolname (:name old-op) (map second binding))
   :returns []
   :args []
   :body (-> old-op
             lift-guards
             :body
             (apply-binding binding)
             unroll-sub)})

(defn- unroll-operation
  [op]
  (let [bindings (b/get-op-combinations (:name op))]
    (if (seq bindings)
      (map (partial new-op op) bindings)
      (list (assoc op :body (unroll-sub (:body op)))))))

(defn- unroll-clause
  [c]
  (match c
         {:tag :variables :values v} {:tag :variables :values (mapcat b/unroll-variable v)}
         {:tag :invariants :values v} {:tag :invariants :values (mapcat unroll-predicate v)}
         {:tag :init :values v} {:tag :init :values (map unroll-sub v)}
         {:tag :operations :values v} {:tag :operations :values (mapcat unroll-operation v)}
         _ c))

(defn- simplify-formula
  [formula]
  (match formula
    {:tag :not :pred {:tag :not :pred p}} p
    {:tag :pred->bool :pred {:tag :equals :left :TRUE :right :FALSE}} :FALSE
    {:tag :pred->bool :pred {:tag :equals :left :TRUE :right :TRUE}} :TRUE
    {:tag :not :pred {:tag :equals :left :TRUE :right :TRUE}} FALSE
    {:tag :not :pred {:tag :equals :left :TRUE :right :FALSE}} TRUE
    {:tag :or :preds (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :preds (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :preds (_ :guard (fn [ps] (some #(= % FALSE) ps)))} FALSE
    {:tag :or :preds (_ :guard (fn [ps] (some #(= % TRUE) ps)))} TRUE
    {:tag :and :preds (ps :guard (fn [ps] (some #(= % TRUE) ps)))} {:tag :and :preds (filter #(not= % TRUE) ps)}
    {:tag :or :preds (ps :guard (fn [ps] (some #(= % FALSE) ps)))} {:tag :or :preds (filter #(not= % FALSE) ps)}
    {:tag :equals :left {:tag :pred->bool :pred p} :right :TRUE} p
    {:tag :assignment :id-vals ([a {:tag :pred->bool :pred {:tag :equals :left b :right :TRUE}}] :seq)} (if (= a b) s/NONE nil)
    {:tag :pred->bool :pred {:tag :add :nums n}} {:tag :add :nums n}
    {:tag :parallel-sub :subs (substitutions :guard #(= (count %) 1))} (first substitutions)
    {:tag :parallel-sub :subs (_ :guard empty?)} s/NONE
    {:tag :select :clauses ([outer-guard {:tag :select :clauses ([inner-guard & r] :seq)}] :seq)} {:tag :select :clauses (cons (AND outer-guard inner-guard) r)}
    _ nil))

(defn- simplify-ir
  [ir]
  (s/transform [(s/walker simplify-formula)] simplify-formula ir))

(defn- simplify-all
  [ir]
  (loop [IR ir]
    (let [next-ir (simplify-ir IR)]
      (if (= IR next-ir)
        next-ir
        (recur next-ir)))))

(defn boolencode
  [ir]
  (b/setup-backend ir)
  (simplify-all {:tag :machine
                 :name (:name ir)
                 :clauses (map unroll-clause (:clauses ir))}))
