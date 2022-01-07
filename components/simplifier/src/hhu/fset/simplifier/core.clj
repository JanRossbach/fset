(ns hhu.fset.simplifier.core
  (:require
   [hhu.fset.dsl.interface :refer [AND FALSE AND TRUE NOT]]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]))

(defn simplify-formula
  [formula]
  (match formula
    {:tag :not :pred {:tag :not :pred p}} p
    {:tag :pred->bool :pred {:tag :equals :left true :right false}} {:tag :false} ;; We can't return false or the walker breaks but we wan't the value false
    {:tag :pred->bool :pred {:tag :equals :left true :right true}} true
    {:tag :not :pred {:tag :equals :left true :right true}} FALSE
    {:tag :not :pred {:tag :equals :left true :right false}} TRUE
    {:tag :or :preds (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :preds (ps :guard #(= 1 (count %)))} (first ps)
    {:tag :and :preds (_ :guard (fn [ps] (some #(= % FALSE) ps)))} FALSE
    {:tag :or :preds (_ :guard (fn [ps] (some #(= % TRUE) ps)))} TRUE
    {:tag :and :preds (ps :guard (fn [ps] (some #(= % TRUE) ps)))} (let [nps (filter #(not= % TRUE) ps)] (if (empty? nps) TRUE {:tag :and :preds nps}))
    {:tag :or :preds (ps :guard (fn [ps] (some #(= % FALSE) ps)))} (let [nps (filter #(not= % FALSE) ps)] (if (empty? nps) FALSE {:tag :or :preds nps}))
    {:tag :equals :left {:tag :pred->bool :pred p} :right true} p
    {:tag :assignment :id-vals ([a {:tag :pred->bool :pred {:tag :equals :left b :right true}}] :seq)} (if (= a b) s/NONE nil)
    {:tag :parallel-sub :subs (substitutions :guard #(= (count %) 1))} (first substitutions)
    {:tag :parallel-sub :subs (_ :guard empty?)} s/NONE
    {:tag :select :clauses ([outer-guard {:tag :select :clauses ([inner-guard & r] :seq)}] :seq)} {:tag :select :clauses (cons (AND outer-guard inner-guard) r)}
    {:tag :if-expr :cond (_ :guard #(= % TRUE)) :then then} then
    {:tag :if-expr :cond (_ :guard #(= % FALSE)) :else else} else
    {:tag :implication :preds ([(_ :guard #(= % TRUE)) B] :seq)} B
    {:tag :implication :preds ([(_ :guard #(= % FALSE)) _] :seq)} TRUE
    {:tag :implication :preds ([_ (_ :guard #(= % TRUE))] :seq)} TRUE
    {:tag :implication :preds ([A (_ :guard #(= % FALSE))] :seq)} (NOT A)
    {:tag :equivalence :preds ([(_ :guard #(= % TRUE)) B] :seq)} B
    {:tag :equivalence :preds ([(_ :guard #(= % FALSE)) B] :seq)} (NOT B)
    {:tag :equivalence :preds ([A (_ :guard #(= % TRUE))] :seq)} A
    {:tag :equivalence :preds ([A (_ :guard #(= % FALSE))] :seq)} (NOT A)
    _ nil))

(defn- simplify-ir
  [ir]
  (s/transform [(s/walker simplify-formula)] (fn [f] (let [sf (simplify-formula f)]
                                                       (if (= sf {:tag :false}) ;; Replace the hacky map with the actual value we want
                                                         false
                                                         sf)))
               ir))

(defn simplify-all
  [ir]
  (loop [IR ir]
    (let [next-ir (simplify-ir IR)]
      (if (= IR next-ir)
        next-ir
        (recur next-ir)))))
