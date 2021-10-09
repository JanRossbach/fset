(ns fset.backend.predicates-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [fset.predicates :refer [transform-predicate]]))

(def and-ir
  {:tag :and,
   :predicates
   '({:tag :member,
      :element :active,
      :set {:tag :power-set, :set :PID}}
     {:tag :member, :element :ready, :set {:tag :power-set, :set :PID}}
     {:tag :member,
      :element :waiting,
      :set {:tag :power-set, :set :PID}}
     {:tag :subset, :subset :active, :set :PID}
     {:tag :subset, :subset :ready, :set :PID}
     {:tag :subset, :subset :waiting, :set :PID}
     {:tag :equal,
      :left {:tag :intersection, :sets (:ready :waiting)},
      :right #{}}
     {:tag :equal,
      :left
      {:tag :intersection,
       :sets (:active {:tag :union, :sets (:ready :waiting)})},
      :right #{}}
     {:tag :less-eq, :numbers ({:tag :card, :set :active} 1)}
     {:tag :implication,
      :predicates
      ({:tag :equal, :left :active, :right #{}}
       {:tag :equal, :left :ready, :right #{}})})})

(deftest predicates-test
  (testing "AND"
    (is (= and-ir (transform-predicate {} and-ir)))))
