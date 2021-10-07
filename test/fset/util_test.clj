(ns fset.util-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [fset.util :as util]))

(def no-clause-ir
  {:clauses '()})

(def one-clause-ir
  {:clauses '({:tag :invariants :predicate nil})})

(def two-clause-ir {:clauses '({:tag :invariants :predicate nil}
                               {:tag :init :substitution nil})})

(deftest clause-utils-test
  (testing "Adding new clauses"
    (is (= one-clause-ir (util/add-clause no-clause-ir {:tag :invariants :predicate nil})))
    (is (= two-clause-ir (util/add-clause-after one-clause-ir {:tag :init :substitution nil})))))

(def no-variables-ir
  {:clauses '()})

(def some-variables-ir
  {:clauses
   '({:tag :variables
      :identifiers (:active :ready :waiting)})})

(def more-variables-ir {:clauses '({:tag :variables
                                    :identifiers (:active :ready :waiting :hello :world)})})

(deftest variable-utils-test
  (testing "Adding variables"
    (is (= more-variables-ir (util/add-vars some-variables-ir '(:hello :world))))
    (is (= some-variables-ir (util/add-vars no-variables-ir '(:active :ready :waiting))))
    (is (= some-variables-ir (util/set-vars more-variables-ir '(:active :ready :waiting))))))


(def no-init-ir {:clauses '()})
(def single-init-ir {:clauses
                     '({:tag :init
                        :substitution {:tag :assign :identifiers (:q) :values (7)}})})
(def multiple-init-ir
  {:clauses
   '({:tag :init
      :substitution {:tag :parallel-substitution
                     :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                     {:tag :assign :identifiers (:r) :values (13)})}})})

(def more-init-ir
  {:clauses
   '({:tag :init
      :substitution {:tag :parallel-substitution
                     :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                     {:tag :assign :identifiers (:r) :values (13)}
                                     {:tag :assign :identifiers (:p) :values (3)})}})})

(def even-more-init-ir
  {:clauses
   '({:tag :init
      :substitution {:tag :parallel-substitution
                     :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                     {:tag :assign :identifiers (:r) :values (13)}
                                     {:tag :assign :identifiers (:p) :values (3)}
                                     {:tag :assign :identifiers (:x) :values (15)}
                                     {:tag :assign :identifiers (:y) :values (false)}
                                     {:tag :assign :identifiers (:z) :values (:VAR)})}})})
(deftest init-utils-test
  (testing "Counting"
    (is (= 0 (util/count-inits no-init-ir)))
    (is (= 1 (util/count-inits single-init-ir)))
    (is (= 2 (util/count-inits multiple-init-ir)))
    (is (= 3 (util/count-inits more-init-ir)))
    (is (= 6 (util/count-inits even-more-init-ir))))
  (testing "Getting"
    (is (= (first (:clauses multiple-init-ir)) (util/get-init multiple-init-ir)))
    (is (empty? (util/get-init some-variables-ir))))
  (testing "Adding"
    (is (= single-init-ir (util/add-inits no-init-ir [[:q 7]])))
    (is (= multiple-init-ir (util/add-inits single-init-ir [[:r 13]])))
    (is (= more-init-ir (util/add-inits multiple-init-ir [[:p 3]])))
    (is (= even-more-init-ir (util/add-inits multiple-init-ir [[:p 3] [:x 15] [:y false] [:z :VAR]])))
    (is (= multiple-init-ir (util/add-inits no-init-ir [[:q 7] [:r 13]]))))
  (testing "Deleting"
    (is (= multiple-init-ir (util/rm-init-by-id more-init-ir :p)))
    (is (= no-init-ir (util/rm-init-by-id single-init-ir :q)))
    (is (= single-init-ir (util/rm-init-by-id multiple-init-ir :r)))
    (is (= no-init-ir (util/rm-init-by-id no-init-ir :p)))
    (is (= multiple-init-ir (util/rm-init-by-id multiple-init-ir :x)))
    (is (= single-init-ir (util/rm-init-by-id single-init-ir :r)))
    (is (= single-init-ir (util/rm-inits-by-id more-init-ir [:p :r])))))


(deftest invar-utils-test
  (testing "Getting"
    ()))
