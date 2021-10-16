(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [ir->b]]
   [fset.core :as fset]))


(def empty-ir {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses nil})

(deftest empty-machine-test
  (testing "The empty machine does not contain the :nil set and should throw an exception."
    (is (= empty-ir (:ir (fset/boolencode 10 3 true empty-ir :nil))))))

(def scheduler-ir {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :scheduler, :parameters []}, :clauses '({:tag :sets, :set-definitions ({:tag :deferred-set, :identifier :PID})} {:tag :variables, :identifiers (:active :ready :waiting)} {:tag :invariants, :predicate {:tag :and, :predicates ({:tag :member, :element :active, :set {:tag :power-set, :set :PID}} {:tag :member, :element :ready, :set {:tag :power-set, :set :PID}} {:tag :member, :element :waiting, :set {:tag :power-set, :set :PID}} {:tag :subset, :subset :active, :set :PID} {:tag :subset, :subset :ready, :set :PID} {:tag :subset, :subset :waiting, :set :PID} {:tag :equal, :left {:tag :intersection, :sets (:ready :waiting)}, :right #{}} {:tag :equal, :left {:tag :intersection, :sets (:active {:tag :union, :sets (:ready :waiting)})}, :right #{}} {:tag :less-eq, :numbers ({:tag :card, :set :active} 1)} {:tag :implication, :predicates ({:tag :equal, :left :active, :right #{}} {:tag :equal, :left :ready, :right #{}})})}} {:tag :init, :substitution {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:active), :values (#{})} {:tag :assign, :identifiers (:ready), :values (#{})} {:tag :assign, :identifiers (:waiting), :values (#{})})}} {:tag :operations, :operations ({:tag :operation, :return [:rr], :name :nr_ready, :parameters [], :body {:tag :assign, :identifiers (:rr), :values ({:tag :card, :set :ready})}} {:tag :operation, :return [], :name :new, :parameters [:pp], :body {:tag :select, :clauses ({:tag :and, :predicates ({:tag :member, :element :pp, :set :PID} {:tag :not, :predicate {:tag :member, :element :pp, :set :active}} {:tag :not, :predicate {:tag :member, :element :pp, :set {:tag :union, :sets (:ready :waiting)}}})} {:tag :assign, :identifiers (:waiting), :values ({:tag :union, :sets (:waiting #{:pp})})})}} {:tag :operation, :return [], :name :del, :parameters [:pp], :body {:tag :select, :clauses ({:tag :member, :element :pp, :set :waiting} {:tag :assign, :identifiers (:waiting), :values ({:tag :minus, :numbers (:waiting #{:pp})})})}} {:tag :operation, :return [], :name :ready, :parameters [:rr], :body {:tag :select, :clauses ({:tag :member, :element :rr, :set :waiting} {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:waiting), :values ({:tag :minus, :numbers (:waiting #{:rr})})} {:tag :if-sub, :condition {:tag :equal, :left :active, :right #{}}, :then {:tag :assign, :identifiers (:active), :values (#{:rr})}, :else {:tag :assign, :identifiers (:ready), :values ({:tag :union, :sets (:ready #{:rr})})}})})}} {:tag :operation, :return [], :name :swap, :parameters [], :body {:tag :select, :clauses ({:tag :not-equal, :left :active, :right #{}} {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:waiting), :values ({:tag :union, :sets (:waiting :active)})} {:tag :if-sub, :condition {:tag :equal, :left :ready, :right #{}}, :then {:tag :assign, :identifiers (:active), :values (#{})}, :else {:tag :any, :identifiers [:pp], :where {:tag :member, :element :pp, :set :ready}, :then {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:active), :values (#{:pp})} {:tag :assign, :identifiers (:ready), :values ({:tag :minus, :numbers (:ready #{:pp})})})}}})})}})})})

(deftest scheduler-machine-test
  (testing "The scheduler machine should be changed in some way"
    (is (not= scheduler-ir (fset/boolencode 10 3 true scheduler-ir :PID)))))


(deftest valid-B-machine-test
  (testing "After the transformation the IR can be translated into a B machine."
    (is (string? (ir->b (:ir (fset/boolencode 10 3 true scheduler-ir :PID)))))))
