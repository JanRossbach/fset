(ns hhu.fset.backend.core
  (:require
   [clojure.core.matrix :as m]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]
   [hhu.fset.backend.util :refer [create-boolname]]
   [hhu.fset.backend.lisb-util :as lu]
   [hhu.fset.backend.specter-util :as su]
   [lisb.translation.util :refer [b->ir]]
   [lisb.core :refer [eval-ir-formula]]
   [lisb.translation.lisb2ir :refer [bmember? bcomprehension-set bexists band b=]]))

(def typestring->ir
  (memoize (fn [type-str]
             (b->ir (str "#EXPRESSION" type-str)))))

(defn get-type-ir
  [ss expr]
  (typestring->ir (lu/get-type ss expr)))

(defn interpret-animator-result
  [result]
  (if (= result :timeout)
    :timeout
    (if (coll? result)
      (match (first result)
             (_ :guard number?) result
             (_ :guard string?) (sort (map keyword result))
             (_ :guard vector?) (map (fn [[l r]] {:tag :maplet :left l :right r}) (sort-by first (map (fn [v] (mapv interpret-animator-result v)) result)))
             (_ :guard set?) (map interpret-animator-result result)
             (_ :guard char?) (keyword result)
             nil '())
      (if (string? result) (keyword result) result))))

(defn comprehend
  [ss ids pred]
  (interpret-animator-result
   (eval-ir-formula
    ss
    (bcomprehension-set ids pred))))

(defn get-set-elems
  [ss set-ir]
  (comprehend ss [:x] {:tag :member :elem :x :set set-ir}))

(defn get-relation-elems
  [ss rdomain rrange]
  (comprehend ss [:x :y] (band (bmember? :x rdomain)
                            (bmember? :y rrange))))

(defn get-type-elems
  [ss expr]
  (get-set-elems ss (get-type-ir ss expr)))

(defn get-sub-type-elems
  [ss expr]
  (match (get-type-ir ss expr)
         {:tag :power-set :set s} (get-set-elems ss s)
         {:tag :relation :sets ([A B] :seq)} (get-relation-elems ss A B)))

(defn get-type-elem-matrix
  [ss expr]
  (match (get-type-ir ss expr)
    {:tag :power-set :set S} (m/matrix (vector (get-set-elems ss S)))
    {:tag :relation :sets ([A B] :seq)}
    (m/matrix (for [a (get-set-elems ss A)]
                (for [b (get-set-elems ss B)]
                  {:tag :maplet :left a :right b})))
    {:tag :cartesian-product-or-multiplication :nums-or-sets ([A B] :seq)}
    (m/matrix (for [a (get-set-elems ss A)]
                (for [b (get-set-elems ss B)]
                  {:tag :maplet :left a :right b})))
    S (m/matrix (vector (get-set-elems ss S)))))

(defn eval-constant [ss ir c]
  (set (first (comprehend ss [:x] (bexists (su/get-constants ir) (band (b= :x c) (su/get-props-as-pred ir)))))))

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
             {:tag :any :pred w} w))
         non-det-clauses)))

(defn get-non-det-ids
  [op]
  (mapcat (fn [clause]
         (match clause
           {:tag :any :ids ids} ids))
       (get-non-det-clauses op)))

(defn type?
  [ir expr]
  (match expr
    (_ :guard (partial su/carrier? ir)) true
    {:tag :relation :sets ([_ _] :seq)} true
    {:tag :power-set :set (_ :guard (partial type? ir))} true
    {:tag :power1-set :set (_ :guard (partial type? ir))} true
    {:tag :fin :set (_ :guard (partial type? ir))} true
    {:tag :fin1 :set (_ :guard (partial type? ir))} true
    _ false))

(defn finite-type?
  [ir ss config expr]
  (if (su/carrier? ir expr)
    true
    (match (get-type-ir ss expr)
           {:tag :power-set :set {:tag :integer-set}} false
           {:tag :integer-set} false
           {:tag :power-set :set (_ :guard (partial finite-type? ir ss config))} true
           {:tag :relation :sets ([A B] :seq)} (and (su/carrier? ir A) (su/carrier? ir B) (< (count (get-relation-elems ss A B)) (:max-unroll-size config)))
      _ false)))

(defn finite?
  [ir ss config expr]
  (match expr
    ;;{:tag :interval :from n :to m} (< (- m n) (:max-unroll-size config))
    {:tag :cardinality} true
    _ (finite-type? ir ss config expr)))

(defn unrollable?
  [ir ss config id]
  (and
   (not (= :all (:excluded-vars config)))
   (not (contains? (:excluded-vars config) id))
   (finite? ir ss config id)))

(defn unrollable-var?
  [ir ss config id]
  (and
   (su/variable? ir id)
   (unrollable? ir ss config id)))

(defn fn-call?
  [expr]
  (match expr
         {:tag :fn-call} :true
         _ :false))

(defn unroll-variable-as-matrix
  [ss var-id rdom rran]
  (let [dom-elems (get-sub-type-elems ss rdom)
        ran-elems (get-sub-type-elems ss rran)]
    (m/matrix (for [d dom-elems]
                (for [r ran-elems]
                  {:name (create-boolname var-id d r)
                   :elem {:tag :maplet :left d :right r}
                   :var var-id})))))

(def unroll-variable
  (memoize
   (fn
     [ir ss config var-id]
     (if (unrollable? ir ss config var-id)
       (let [TIR (get-type-ir ss var-id)]
         (flatten (match TIR
                    {:tag :power-set :set (_ :guard (partial su/carrier? ir))}
                    (mapv (fn [elem] {:name (create-boolname var-id elem)
                                      :elem elem
                                      :var var-id})
                          (get-sub-type-elems ss var-id))
                    {:tag :relation :sets ([A B] :seq)}
                    (unroll-variable-as-matrix ss var-id A B))))
       (list {:name var-id :elem nil :var var-id})))))

(defn get-all-bools
  [ir ss config]
  (let [vars (su/get-vars ir)
        svars (into #{} vars)]
    (filter (fn [v] (not (contains? svars (:name v)))) (mapcat (fn [id] (unroll-variable ir ss config id)) vars))))

(defn elem->bools
  [ss ir elem]
  (let [elem-type (typestring->ir (lu/get-type ss elem))
        vars (first (su/get-vars ir))]
    (mapcat
     (fn [v]
       (match (typestring->ir (lu/get-type ss v))
         {:tag :power-set :set (_ :guard #(= % elem-type))} (list (create-boolname v elem))
         {:tag :relation :sets ([(_ :guard #(= % elem-type)) B] :seq)} (map (fn [el2] (create-boolname v elem el2)) (get-set-elems ss B))
         {:tag :relation :sets ([A (_ :guard #(= % elem-type))] :seq)} (map (fn [el1] (create-boolname v el1 elem)) (get-set-elems ss A))
         _ '()))
     vars)))

(defn guard?
  [ir id]
  (match ir
         {:tag :not :pred {:tag :member :elem (_ :guard #(= % id)) :set _}} true
         {:tag :member :elem (_ :guard #(= % id)) :set _} true
         _ false))

(defn get-elem-index [ss elem-id]
  (let [elems (if (map? elem-id) (flatten (get-type-elem-matrix ss elem-id)) (get-type-elems ss elem-id))]
    (first (first (filter #(= (second %) elem-id) (map-indexed vector elems))))))

(defn find-guards
  [op id]
   (s/select [(s/codewalker #(guard? % id))] op))

(defn get-pred-type [pred]
  (match pred
         {:tag :and :preds ps} (get-pred-type (first ps))
         {:tag :not :pred p} (get-pred-type p)
         {:tag :member :elem _ :set S} S))

(defn get-param-elems [ir ss id]
  (let [guards (apply band (find-guards ir id))
        Type (get-pred-type guards)]
      (get-sub-type-elems ss Type)))

(defn combine
  [bindings [id elems]]
  (if (empty? bindings)
    (mapv (fn [elem] [[id elem]]) elems)
    (for [b bindings
          e elems]
      (conj b [id e]))))

(defn ids->bindings [ir ss ids]
  (let [idbinds (map (fn [id] (list id (get-param-elems ir ss id))) ids)]
    (reduce combine [] idbinds)))

(def op->bindings
  (memoize
   (fn [ss op]
     (let [ids (concat (:args op) (get-non-det-ids op))]
       (ids->bindings op ss ids)))))

(defn unrollable-op?
  [ss op]
  (try
    (and
     (empty? (:returns op))
     (seq (op->bindings ss op)))
    (catch Exception e
      false)))

(defn unrollable-param?
  [ir ss op id]
  (unrollable-op? ss op))
