(ns fset.simplify
  (:require
   [fset.dsl :refer [AND FALSE AND TRUE]]
   [fset.backend :as b]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as s]))

(defn simplify-formula
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

(defn simplify-all
  [ir]
  (loop [IR ir]
    (let [next-ir (simplify-ir IR)]
      (if (= IR next-ir)
        next-ir
        (recur next-ir)))))
