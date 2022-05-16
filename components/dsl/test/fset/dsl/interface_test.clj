(ns fset.dsl.interface-test
  (:require
   [lisb.translation.util :refer [ir->b]]
   [clojure.test :refer [are deftest testing]]
   [fset.dsl.interface :as dsl]))

(deftest lisb-ir-return-test
  (testing "Lisb functions still return the expected IR."
    (are [x y] (= x y)
      {:tag :equals, :left true, :right true} dsl/TRUE
      {:tag :equals, :left true, :right false} dsl/FALSE
      {:tag :pred->bool, :pred {:tag :equals, :left true, :right true}} (dsl/BOOL dsl/TRUE)
      {:tag :and, :preds '({:tag :equals, :left true, :right true}
                           {:tag :equals, :left true, :right false})}
      (dsl/AND dsl/TRUE dsl/FALSE)

      {:tag :or, :preds '({:tag :equals, :left true, :right true}
                          {:tag :equals, :left true, :right false})}
      (dsl/OR dsl/TRUE dsl/FALSE)
      {:tag :not, :pred {:tag :equals, :left true, :right true}}
      (dsl/NOT dsl/TRUE)
      {:tag :equivalence, :preds '({:tag :equals, :left true, :right true}
                                   {:tag :equals, :left true, :right false})}
      (dsl/<=> dsl/TRUE dsl/FALSE)
      {:tag :implication, :preds '({:tag :equals, :left true, :right false}
                                   {:tag :equals, :left true, :right true})}
      (dsl/=> dsl/FALSE dsl/TRUE)
      {:tag :equals, :left :activePID1, :right {:tag :pred->bool, :pred {:tag :equals, :left true, :right false}}}
      (dsl/EQUALS :activePID1 (dsl/BOOL dsl/FALSE))
      {:tag :equals, :left :active, :right true}
      (dsl/=TRUE :active)
      {:tag :member, :elem :activePID1, :set :active}
      (dsl/IN :activePID1 :active)
      {:tag :member, :elem :activePID1, :set {:tag :bool-set}}
      (dsl/BOOLDEF :activePID1)
      {:tag :and, :preds '({:tag :member, :elem :activePID1, :set {:tag :bool-set}} {:tag :member, :elem :activePID2, :set {:tag :bool-set}})}
      (dsl/BOOLDEFS [:activePID1 :activePID2])
      {:tag :if-expr, :cond {:tag :equals, :left true, :right true}, :then :activePID1, :else :activePID2}
      (dsl/IF dsl/TRUE :activePID1 :activePID2)
      {:tag :assignment, :id-vals '(:activePID1 true)}
      (dsl/ASSIGN :activePID1 true)
      {:tag :machine, :name :empty, :machine-clauses [] :args []}
      (dsl/MACHINE :empty [])
      {:tag :union, :sets '({:tag :if-expr, :cond {:tag :equals, :left :activePID1, :right true}, :then #{:PID1}, :else #{}}
                            {:tag :if-expr, :cond {:tag :equals, :left :activePID2, :right true}, :then #{:PID2}, :else #{}}
                            {:tag :if-expr, :cond {:tag :equals, :left :activePID3, :right true}, :then #{:PID3}, :else #{}})}
      (dsl/bv->setexpr [{:name :activePID1 :elem :PID1}
                        {:name :activePID2 :elem :PID2}
                        {:name :activePID3 :elem :PID3}]))))

(deftest resulting-ir-can-be-turned-back-to-string-test
  (are [x] (string? (ir->b x))
    dsl/TRUE
    dsl/FALSE
    (dsl/BOOL dsl/TRUE)
    (dsl/NOT dsl/TRUE)
    (dsl/AND dsl/TRUE dsl/TRUE)
    (dsl/OR dsl/TRUE dsl/FALSE)
    (dsl/<=> dsl/TRUE dsl/FALSE)
    (dsl/=> dsl/TRUE dsl/FALSE)
    (dsl/EQUALS :activePID1 (dsl/BOOL dsl/FALSE))
    (dsl/=TRUE :active)
    (dsl/IN :activePID1 :active)
    (dsl/BOOLDEF :activePID1)
    (dsl/BOOLDEFS [:activePID1 :activePID2])
    (dsl/IF dsl/TRUE :activePID1 :activePID2)
    (dsl/ASSIGN :activePID1 true)
    (dsl/MACHINE :empty [])
    (dsl/bv->setexpr [{:name :activePID1 :elem :PID1}
                      {:name :activePID2 :elem :PID2}
                      {:name :activePID3 :elem :PID3}])))
