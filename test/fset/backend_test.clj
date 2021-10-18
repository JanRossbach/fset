(ns fset.backend-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.util :as util]
   [fset.backend :as b]))

 (def scheduler-ir {:tag :machine, :variant {:tag :machine-variant}, :header {:tag :machine-header, :name :scheduler, :parameters []}, :clauses '({:tag :sets, :set-definitions ({:tag :enumerated-set, :identifier :PID, :elements (:PID1 :PID2 :PID3)})} {:tag :variables, :identifiers (:active :ready :waiting)} {:tag :invariants, :predicate {:tag :and, :predicates ({:tag :member, :element :active, :set {:tag :power-set, :set :PID}} {:tag :member, :element :ready, :set {:tag :power-set, :set :PID}} {:tag :member, :element :waiting, :set {:tag :power-set, :set :PID}} {:tag :subset, :subset :active, :set :PID} {:tag :subset, :subset :ready, :set :PID} {:tag :subset, :subset :waiting, :set :PID} {:tag :equal, :left {:tag :intersection, :sets (:ready :waiting)}, :right #{}} {:tag :equal, :left {:tag :intersection, :sets (:active {:tag :union, :sets (:ready :waiting)})}, :right #{}} {:tag :less-eq, :numbers ({:tag :card, :set :active} 1)} {:tag :implication, :predicates ({:tag :equal, :left :active, :right #{}} {:tag :equal, :left :ready, :right #{}})})}} {:tag :init, :substitution {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:active), :values (#{})} {:tag :assign, :identifiers (:ready), :values (#{})} {:tag :assign, :identifiers (:waiting), :values (#{})})}} {:tag :operations, :operations ({:tag :operation, :return [:rr], :name :nr_ready, :parameters [], :body {:tag :assign, :identifiers (:rr), :values ({:tag :card, :set :ready})}} {:tag :operation, :return [], :name :new, :parameters [:pp], :body {:tag :select, :clauses ({:tag :and, :predicates ({:tag :member, :element :pp, :set :PID} {:tag :not, :predicate {:tag :member, :element :pp, :set :active}} {:tag :not, :predicate {:tag :member, :element :pp, :set {:tag :union, :sets (:ready :waiting)}}})} {:tag :assign, :identifiers (:waiting), :values ({:tag :union, :sets (:waiting #{:pp})})})}} {:tag :operation, :return [], :name :del, :parameters [:pp], :body {:tag :select, :clauses ({:tag :member, :element :pp, :set :waiting} {:tag :assign, :identifiers (:waiting), :values ({:tag :minus, :numbers (:waiting #{:pp})})})}} {:tag :operation, :return [], :name :ready, :parameters [:rr], :body {:tag :select, :clauses ({:tag :member, :element :rr, :set :waiting} {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:waiting), :values ({:tag :minus, :numbers (:waiting #{:rr})})} {:tag :if-sub, :condition {:tag :equal, :left :active, :right #{}}, :then {:tag :assign, :identifiers (:active), :values (#{:rr})}, :else {:tag :assign, :identifiers (:ready), :values ({:tag :union, :sets (:ready #{:rr})})}})})}} {:tag :operation, :return [], :name :swap, :parameters [], :body {:tag :select, :clauses ({:tag :not-equal, :left :active, :right #{}} {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:waiting), :values ({:tag :union, :sets (:waiting :active)})} {:tag :if-sub, :condition {:tag :equal, :left :ready, :right #{}}, :then {:tag :assign, :identifiers (:active), :values (#{})}, :else {:tag :any, :identifiers [:pp], :where {:tag :member, :element :pp, :set :ready}, :then {:tag :parallel-substitution, :substitutions ({:tag :assign, :identifiers (:active), :values (#{:pp})} {:tag :assign, :identifiers (:ready), :values ({:tag :minus, :numbers (:ready #{:pp})})})}}})})}})})})

(deftest loading-statespace-test
  (testing "The returned statespace has the correct Type."
      (is (= de.prob.statespace.StateSpace (type (b/get-statespace scheduler-ir))))))


(def scheduler-ss (b/get-statespace scheduler-ir))


(deftest set-elems-test
  (testing "The correct elements are returned."
   (is (= #{"PID1" "PID2" "PID3"} (b/set-elems scheduler-ss :PID)))))

(deftest type-test
  (testing "Powerset Type"
    (is (= "POW(PID)" (b/get-type scheduler-ss :active)))
    (is (= "POW(PID)" (b/get-type scheduler-ss :ready)))
    (is (= "POW(PID)" (b/get-type scheduler-ss :waiting)))))


(def scheduler-invar (util/get-invariant scheduler-ir))
