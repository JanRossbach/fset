(ns fset.core
  (:require
   [fset.dsl :refer [MACHINE AND OR =TRUE <=> NOT TRUE FALSE EQUALS => BOOL BOOLDEFS IF ASSIGN]]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [fset.backend :as b]))


(defn- replace-param
  [ir [parameter element]]
  (s/setval [(s/walker #(= % parameter))] element ir))

(defn- replace-params
  [ir id-vals]
  (reduce replace-param ir id-vals))

(defn- type?
  [expr]
  (match expr
    (_ :guard b/carrier?) true
    {:tag :relation :sets ([_ _] :seq)} true
    {:tag :power-set :set (_ :guard type?)} true
    {:tag :power1-set :set (_ :guard type?)} true
    {:tag :fin :set (_ :guard type?)} true
    {:tag :fin1 :set (_ :guard type?)} true
    _ false))


(defn setexpr->bitvector
  [set-expr]
  ((fn T [e]
     (match e
       #{} (repeat (b/get-max-size) FALSE)
       (enumeration-set :guard set?) (map (fn [e] (if (contains? enumeration-set e) TRUE FALSE)) (b/get-all-elems-from-elem (first enumeration-set)))
       {:tag :comprehension-set :ids ids :preds preds} '() ;; FIXMEEEE set-comprehension
       ;; FIXME Lambda expressions
       {:tag :union :sets ([A B] :seq)} (map (fn [a b] (OR a b)) (T A) (T B))
       {:tag :intersection :sets ([A B] :seq)} (map (fn [a b] (AND a b)) (T A) (T B))
       {:tag :difference :sets ([A B] :seq)} (map (fn [a b] (AND a (NOT b))) (T A) (T B))
       {:tag :unite-sets :set-of-sets ss} (apply map OR (map T ss))
       {:tag :intersect-sets :set-of-sets ss} (apply map AND (map T ss))
       {:tag :sub :nums ns} (T {:tag :difference :sets ns})
       {:tag :integer-set} (list e)

       {:tag :inverse :rel r} (b/invert-bitvector (T r))
       {:tag :image :rel r :set s} (b/image (T r) s)

       (variable :guard b/variable?) (map =TRUE (b/unroll-variable variable))
       _ (list e)))
   set-expr))

(defn intexpr->intexpr
  [intexpr]
  (match intexpr
    {:tag :cardinality :set s} {:tag :add :nums (map (fn [p] (IF (=TRUE (BOOL p)) 1 0)) (setexpr->bitvector s))}
    _ intexpr))

(defn- unroll-expression
  [expr]
  (match expr
         (_ :guard b/setexpr?) (setexpr->bitvector expr)
         (_ :guard b/intexpr?) (intexpr->intexpr expr)
         _ expr))

(defn- unroll-predicate
  [pred]
  ((fn T [e]
     (match e
         ;; equalsity
       {:tag :equals :left l :right #{}} (apply AND (map NOT (T l)))
       {:tag :equals :left #{} :right r} (apply AND (map NOT (T r)))
       {:tag :equals :left (l :guard b/setexpr?) :right (r :guard b/setexpr?)}  (apply AND (map <=> (T l) (T r)))
       {:tag :not-equals :left l :right r} (NOT (T (EQUALS l r)))
       {:tag :subset :sets ([(_ :guard b/unrollable-var?) (_ :guard type?)] :seq)} {} ;; An empty map just means delete this thing
       {:tag :subset :sets ([s S] :seq)} (apply AND (map (fn [a b] (=> a b)) (T s) (T S)))
       {:tag :subset-strict :subset s :set S} (let [Ts (T s) TS (T S)] (apply AND (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                                        (map (fn [a b] (=> a b)) Ts TS))))
         ;; logical operators
       {:tag :and :preds ps} (apply AND (map T ps))
       {:tag :or :preds ps} (apply OR (map T ps))
       {:tag :not :pred p} (NOT (T p))
       {:tag :equivalence :preds ([A B] :seq)} (<=> (T A) (T B))
       {:tag :implication :preds ([A B] :seq)} (=> (T A) (T B))

       ;; Quantifiers
       {:tag :for-all :ids ids :implication {:tag :implication :preds ([P Q] :seq)}} (apply AND (apply map (fn [& es]
                                                                                                             (=> (T (replace-params P (map (fn [id e] [id e]) ids es)))
                                                                                                                 (T (replace-params Q (map (fn [id e] [id e]) ids es)))))
                                                                                                       (map (partial b/get-param-elems P) ids)))

       ;; FIXMEE Exists
       ;; Member
       {:tag :member :elem (_ :guard b/set-element?) :set (_ :guard type?)} TRUE
       {:tag :member :elem (el-id :guard b/set-element?) :set v} (first (b/pick-bool-var (setexpr->bitvector v) el-id))
       {:tag :member :elem (_ :guard b/unrollable-var?) :set (_ :guard type?)} {}
       {:tag :member} e

       ;; Numbers
       {:tag :equals :left l :right r} {:tag :equals :left (intexpr->intexpr l) :right (intexpr->intexpr r)}
       {:tag :less-equals :nums ns} {:tag :less-equals :nums (map intexpr->intexpr ns)}
       {:tag :less :nums ns} {:tag :less :nums (map intexpr->intexpr ns)}
       {:tag :greater :nums ns} {:tag :greater :nums (map intexpr->intexpr ns)}
       {:tag :greater-equals :nums ns} {:tag :greater-equals :nums (map intexpr->intexpr ns)}
       expr (unroll-expression expr)))
   pred))

(defn unroll-id-val
  [[id val]]
  (if (b/unrollable-var? id)
    (map (fn [v p] (ASSIGN v (BOOL p)))
         (b/unroll-variable id)
         (unroll-expression val))
    (list (ASSIGN id (unroll-expression val)))))

(defn- unroll-sub
  [sub]
  ((fn T [e]
     (match e
       {:tag :skip} (list {:tag :skip})
       {:tag :parallel-sub :subs substitutions} {:tag :parallel-sub :subs (map T substitutions)}
       {:tag :assignment :id-vals id-vals} {:tag :parallel-sub :subs (mapcat unroll-id-val (partition 2 id-vals))}
       {:tag :if-sub :cond condition :then then :else else} {:tag :if-sub :cond (unroll-predicate condition) :then (T then) :else (T else)}
       {:tag :select :clauses clauses} {:tag :select :clauses (mapcat (fn [[P S]] [(unroll-predicate P) (T S)]) (partition 2 clauses))}
       {:tag :any :ids _ :pred _ :subs then} {:tag :parallel-sub :subs (map T then)}
       _ e))
   sub))


(defn- apply-binding
  [body binding]
  (reduce replace-param body binding))


(defn- add-guards
  [op guards]
  (assoc op :body {:tag :select :clauses (list (apply AND guards) (:body op))}))

(defn- lift-guards
  [op]
  (let [guards (b/get-non-det-guards op)]
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
  (let [bindings (b/op->bindings op)]
    (if (seq bindings)
      (map (partial new-op op) bindings)
      (list (assoc op :body (unroll-sub (:body op)))))))

(defn- unroll-clause
  [c]
  (match c
    {:tag :variables :values v} {:tag :variables :values (mapcat b/unroll-variable v)}
    {:tag :invariants :values v} {:tag :invariants :values (filter #(not= % {}) (cons (BOOLDEFS (b/get-all-bools)) (map unroll-predicate v)))}
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
    {:tag :pred->bool :pred (nonpred :guard #(not (b/predicate? %)))} nonpred
    {:tag :parallel-sub :subs (substitutions :guard #(= (count %) 1))} (first substitutions)
    {:tag :parallel-sub :subs (_ :guard empty?)} s/NONE
    {:tag :select :clauses ([outer-guard {:tag :select :clauses ([inner-guard & r] :seq)}] :seq)} {:tag :select :clauses (cons (AND outer-guard inner-guard) r)}
    {:tag :implication :preds ([(_ :guard #(= % TRUE)) B] :seq)} B
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

(defn unroll-machine
  [{:keys [name clauses]}]
  (MACHINE name (map unroll-clause clauses)))

(defn boolencode
  [ir]
  (->> ir
       (b/setup-backend)
       (unroll-machine)
       (simplify-all)))
