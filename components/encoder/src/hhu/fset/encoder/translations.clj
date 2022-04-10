(ns hhu.fset.encoder.translations
  (:require
   [clojure.core.matrix :as m]
   [taoensso.timbre :as log]
   [com.rpl.specter :as s]
   [clojure.core.match :refer [match]]
   [hhu.fset.dsl.interface :refer [AND OR =TRUE NOT TRUE FALSE BOOL IN CARDINALITY bv->setexpr EQUALS ASSIGN <=> =>]]
   [hhu.fset.backend.interface :as b]))

(declare unroll-predicate setexpr->bitvector1 setexpr->bitvector2)

(defn bmtranspose [bm]
  (apply (partial mapv vector) bm))

(defn bmadd [& bms]
  (apply m/emap (fn [& bs] (apply OR bs)) bms))

(defn bemul [& bms]
  (apply m/emap (fn [& bs] (apply AND bs)) bms))

(defn bmdiff [& bms]
  (apply m/emap (fn [a & bs] (apply AND a (map NOT bs))) bms))

(defn bmmul [a b]
  (let [nested-for (fn [f x y] (mapv (fn [a] (mapv (fn [b] (f a b)) y)) x))]
    (nested-for (fn [x y] (apply OR (mapv AND x y))) a (bmtranspose b))))

(defn bemap [f & bms]
  (apply mapv (fn [& rows] (apply mapv (fn [& elems] (apply f elems)) rows)) bms))

(defn setexpr->bitvector
  [set-expr]
  (flatten (setexpr->bitvector1 set-expr)))

(defn setexpr->bitvector1
  "
  The Expression translation function (T_e)
  Takes a set expression and returns a predicate sequence or cardinality |Type(set-expr)|.
  If the predicate evaluates to true, the element is in the given set.
  If the given IR expression is infinite or not supported, an exception is thrown.

  In: IR map representing a set expression.
  Out: IR pred seq
  "
  [set-expr]
  (let [T setexpr->bitvector1
        e set-expr]
    (match (if (not (b/contains-vars? e)) (b/eval-constant-formula e) e)
           (set-elem :guard b/set-element?) (T #{set-elem})
           #{} (repeat (b/max-unroll-size) FALSE)
           (_ :guard b/carrier?) (repeat (b/max-unroll-size) TRUE)

           {:tag :comprehension-set :ids ([id] :seq) :pred pred} (bemap (fn [elem] (unroll-predicate (b/apply-binding pred [[id elem]]))) (b/get-type-elem-matrix e))

           (enumeration-set :guard #(and (set? %) (every? b/set-element? %)))
           (vector (mapv (fn [el] (if (contains? enumeration-set el) TRUE FALSE)) (b/get-type-elems (first enumeration-set))))

           (enumeration-relation :guard #(and (set? %) (b/simple-tuple? (first %))))
           (bemap (fn [el] (if (contains? enumeration-relation el) TRUE FALSE)) (b/get-type-elem-matrix (first enumeration-relation)))

           (enumeration-set :guard #(and (set? %) (= 1 (count %)) (every? b/fn-call? %)))
           (T (first enumeration-set))

           ;; Operators
           {:tag :union :sets ([A B] :seq)} (bmadd (T A) (T B))
           {:tag :intersection :sets ([A B] :seq)} (bemul (T A) (T B))
           {:tag :difference :sets ([A B] :seq)} (bmdiff (T A) (T B))
           {:tag :unite-sets :set-of-sets ss} (apply bmadd (map T ss))
           {:tag :intersect-sets :set-of-sets ss} (apply bemul (map T ss))
           {:tag :sub :nums ns} (T {:tag :difference :sets ns})

           ;; Variables
           (variable :guard b/unrollable-var?)
           (bemap (fn [elem] (=TRUE (b/create-boolname variable elem))) (b/get-type-elem-matrix variable))

           (c :guard #(and (b/constant? %) (b/unrollable? %)))
           (let [ec (b/eval-constant c)] (bemap (fn [elem] (if (contains? ec elem) TRUE FALSE)) (b/get-type-elem-matrix c)))
           expr (setexpr->bitvector2 expr))))

(defn setexpr->bitvector2
  "Extension for patterns in the setexpr->bitvector function to avoid
  pattern matching Problems with aot-compiling for the cli."
  [expr]
  (let [T setexpr->bitvector1]
    (match expr
        ;; Relations
           {:tag :cartesian-product :sets sets} (reduce bmadd (map T sets))
           {:tag :id :set sete} (bemap (fn [{:keys [left right]}] (if (= left right) TRUE FALSE)) (b/get-type-elem-matrix sete))
           {:tag :inverse :rel r} (bmtranspose (T r))
           {:tag :image :rel r :set s} (bmmul (T s) (T r))
           {:tag :fn-call :f f :args ([(args :guard b/set-element?)] :seq)} (T {:tag :image :rel f :set #{args}})
           {:tag :fn-call :f f :args ([args] :seq)} (T {:tag :image :rel f :set args})
           {:tag :dom :rel r} (vector (mapv (fn [row] (apply OR row)) (T r)))
           {:tag :ran :rel r} (T {:tag :dom :rel {:tag :inverse :rel r}})
           {:tag :domain-restriction :set s :rel r} (mapv (fn [el row] (mapv (fn [elem] (AND el elem)) row)) (first (T s)) (T r))
           {:tag :range-restriction :set s :rel r} (bmtranspose (T {:tag :domain-restriction :set s :rel {:tag :inverse :rel r}}))
           {:tag :domain-subtraction :set s :rel r} (mapv (fn [el row] (mapv (fn [elem] (AND (NOT el) elem)) row)) (first (T s)) (T r))
           {:tag :range-subtraction :set s :rel r} (bmtranspose (T {:tag :domain-subtraction :set s :rel {:tag :inverse :rel r}}))
           {:tag :composition :rels rels} (reduce bmmul (map T rels))
           {:tag :iterate :rel rel :num num} (let [Tr (T rel)
                                                   it0 (T {:tag :union :sets [{:tag :dom :rel rel} {:tag :ran :rel rel}]})]
                                               (nth (iterate (partial bmmul Tr) it0) num))
           {:tag :override :rels ([A B] :seq)} (T {:tag :union :sets [B {:tag :domain-subtraction :set {:tag :dom :rel B} :rel A}]})
           {:tag :lambda :ids ([id] :seq) :pred pred :expr expr} (bemap (fn [{:keys [left right]}] (AND
                                                                                               (unroll-predicate (b/apply-binding pred [[id left]]))
                                                                                               (nth (T expr) (b/get-elem-index right))))
                                                                   (b/get-type-elem-matrix expr))

           _ (throw (ex-info "Unsupported Expression found!" {:failed-because expr})))))

(defn intexpr->intexpr
  [intexpr]
  (match intexpr
    {:tag :cardinality :set s} (CARDINALITY (fn [p] (=TRUE (BOOL p))) (setexpr->bitvector s))
    _ intexpr))

(defn unroll-expression
  [expr]
  (match expr
    (_ :guard b/setexpr?) (setexpr->bitvector expr)
    (_ :guard b/intexpr?) (intexpr->intexpr expr)
    _ expr))

(def boolvar->set (comp bv->setexpr b/unroll-variable))

(defn boolvars->set [ir]
  (s/transform [(s/walker b/unrollable-var?)] boolvar->set ir))

(defn unroll-predicate
  "
  The predicate Translation function (T_p)
  Takes an IR predicate and returns the another, encoded, IR predicate.
  "
  [pred]
  (try
    ((fn T [e]
       (match e
         ;; equality
         {:tag :equals :left l :right #{}} (apply AND (map NOT (T l)))
         {:tag :equals :left #{} :right r} (apply AND (map NOT (T r)))

         {:tag :not-equals :left l :right r} (NOT (T (EQUALS l r)))
         {:tag :subset :sets ([s S] :seq)} (apply AND (map (fn [a b] (=> a b)) (T s) (T S)))
         {:tag :strict-subset :sets ([s S] :seq)} (let [Ts (T s) TS (T S)] (apply AND (cons (apply OR (map (fn [a b] (AND a (NOT b))) Ts TS))
                                                                                            (map (fn [a b] (=> a b)) Ts TS))))
         {:tag :equals :left {:tag :fn-call :f f :args ([{:tag :fn-call :f g :args ([elem] :seq)}] :seq)} :right (r :guard b/set-element?)}
         (T {:tag :equals :left {:tag :image :rel f :set {:tag :image :rel g :set #{elem}}} :right #{r}})

         {:tag :equals :left (l :guard b/setexpr?) :right (r :guard b/setexpr?)}  (apply AND (map <=> (T l) (T r)))
         ;; logical operators
         {:tag :and :preds ps} (apply AND (map T ps))
         {:tag :or :preds ps} (apply OR (map T ps))
         {:tag :not :pred p} (NOT (T p))
         {:tag :equivalence :preds ([A B] :seq)} (<=> (T A) (T B))
         {:tag :implication :preds ([A B] :seq)} (=> (T A) (T B))

         ;; Quantifiers
         {:tag :for-all :ids ids :implication {:tag :implication :preds ([P Q] :seq)}}
         (apply AND (map (fn [binding] (=> (T (b/apply-binding P binding))
                                           (T (b/apply-binding Q binding))))
                         (b/ids->bindings P ids)))

         {:tag :exists :ids ids :pred P}
         (apply OR (map (fn [binding] (T (b/apply-binding P binding))) (b/ids->bindings P ids)))

         ;; Member
         {:tag :member :elem (_ :guard b/unrollable-var?) :set (_ :guard b/type?)} TRUE
         {:tag :member :elem (elem :guard #(or (b/set-element? %) (b/simple-tuple? %))) :set s} (=TRUE (BOOL (nth (T s) (b/get-elem-index elem))))
         {:tag :member :elem {:tag :fn-call :f f :args ([(arg :guard b/set-element?)] :seq)} :set S} (T {:tag :subset :sets (list {:tag :image :rel f :set #{arg}} S)})
         {:tag :member :elem elem :set (S :guard b/unrollable-var?)} (T {:tag :subset :sets (list #{elem} S)})

         ;; Numbers
         {:tag :equals :left l :right r} {:tag :equals :left (T l) :right (T r)}
         {:tag :less-equals :nums ns} {:tag :less-equals :nums (map T ns)}
         {:tag :less :nums ns} {:tag :less :nums (map T ns)}
         {:tag :greater :nums ns} {:tag :greater :nums (map T ns)}
         {:tag :greater-equals :nums ns} {:tag :greater-equals :nums (map T ns)}

         expr (unroll-expression expr)))
     pred)
    (catch Exception e
      (log/info (str "Failed at Predicate: " pred) e)
      (boolvars->set pred))))

(defn unpack-fn-override
  [idval]
  (match idval
    ([{:tag :fn-call :f (fid :guard b/unrollable-var?) :args ([(x :guard b/set-element?)] :seq)} (val :guard b/set-element?)] :seq)
    (list fid {:tag :override :rels (list fid #{{:tag :maplet :left x :right val}})})
    _ idval))

(defn unroll-id-val
  [idval]
  (let [[id val] (unpack-fn-override idval)]
    (if (b/unrollable-var? id)
      (let [uvar (b/unroll-variable id)]
        (try (let [uexpr (unroll-expression val)]
               (map (fn [v p] (ASSIGN v (BOOL p)))
                    (map :name uvar)
                    uexpr))
             (catch Exception e
               (log/info (str "Failed at Assignment: " id val) e)
               (map (fn [v p] (ASSIGN v (BOOL p)))
                    (map :name uvar)
                    (map (fn [v] (IN (:elem v) (boolvars->set val))) uvar)))))
      (list (ASSIGN id (boolvars->set val))))))

(defn unroll-becomes
  [id pred]
  (if (b/unrollable-var? id)
    (let [uvar (b/unroll-variable id)
          upred (unroll-predicate pred)]
      (map (fn [v] {:tag :becomes-such :ids (list v) :pred upred})
           (map :name uvar)))
    (list {:tag :becomes-such :ids (list id) :pred (boolvars->set pred)})))


(defn unroll-sub
  [sub op-unroll]
  (if (not (b/unroll-sub?))
    sub
    ((fn T [e]
       (match e
         {:tag :parallel-sub :subs substitutions} {:tag :parallel-sub :subs (map T substitutions)}
         {:tag :sequential-sub :subs substitutions} {:tag :sequential-sub :subs (map T substitutions)}
         {:tag :let-sub :id-vals id-vals :subs subs} {:tag :parallel-sub :subs (map (fn [sub] (T (b/apply-binding sub (partition 2 id-vals)))) subs)}
         {:tag :precondition :pred pred :subs substitutions} {:tag :precondition :pred (unroll-predicate pred) :subs (map T substitutions)}
         {:tag :assert :pred pred :subs substitutions} {:tag :assert :pred (unroll-predicate pred) :subs (map T substitutions)}
         {:tag :choice :subs substitutions} {:tag :choice :subs (map T substitutions)}
         {:tag :if-sub :cond condition :then then :else else} {:tag :if-sub :cond (unroll-predicate condition) :then (T then) :else (T else)}
         {:tag :select :clauses clauses} {:tag :select :clauses (mapcat (fn [[P S]] [(unroll-predicate P) (T S)]) (partition 2 clauses))}
         {:tag :any :ids ids :pred pred :subs then}
         (if op-unroll
           {:tag :select :clauses (list (unroll-predicate pred) {:tag :parallel-sub :subs (map T then)})}
           {:tag :any :ids ids :pred (unroll-predicate pred) :subs (map T then)})
         {:tag :assignment :id-vals id-vals} {:tag :parallel-sub :subs (mapcat unroll-id-val (partition 2 id-vals))}
         {:tag :becomes-element-of :ids ids :set sete} {:tag :becomes-element-of :ids ids :set (boolvars->set sete)}
         {:tag :becomes-such :ids ([id] :seq) :pred pred} {:tag :parallel-sub :subs (unroll-becomes id pred)}
         _ e))
     sub)))
