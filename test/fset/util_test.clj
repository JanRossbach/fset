(ns fset.util-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [fset.util :as util]))

(def no-clause-ir
  {:clauses '()})

(def one-clause-ir
  {:clauses '({:tag :invariants :values nil})})

(def two-clause-ir {:clauses '({:tag :invariants :values nil}
                               {:tag :init :values nil})})

(deftest clause-utils-test
  (testing "Adding new clauses"
    (is (= one-clause-ir (util/add-clause no-clause-ir {:tag :invariants :values nil})))
    (is (= two-clause-ir (util/add-clause-after one-clause-ir {:tag :init :values nil})))))

(def no-variables-ir
  {:clauses '()})

(def one-variable-ir
  {:clauses '({:tag :variables
               :values (:active)})})

(def some-variables-ir
  {:clauses
   '({:tag :variables
      :values (:active :ready :waiting)})})

(def more-variables-ir {:clauses '({:tag :variables
                                    :values (:active :ready :waiting :hello :world)})})

(deftest variable-utils-test
  (testing "Getting"
    (is (= '(:active :ready :waiting) (util/get-vars some-variables-ir)))
    (is (empty? (util/get-vars no-variables-ir))))
  (testing "Setting"
    (is (= one-variable-ir (util/set-vars some-variables-ir '(:active)))))
  (testing "Adding variables"
    (is (= more-variables-ir (util/add-vars some-variables-ir '(:hello :world))))
    (is (= some-variables-ir (util/add-vars no-variables-ir '(:active :ready :waiting))))
    (is (= some-variables-ir (util/set-vars more-variables-ir '(:active :ready :waiting)))))
  (testing "Removing"
    (is (= no-variables-ir (util/rm-var-by-id one-variable-ir :active)))))

(def no-init-ir {:clauses '()})
(def single-init-ir {:clauses
                     '({:tag :init
                        :values ({:tag :assign :identifiers (:q) :values (7)})})})



(def multiple-init-ir
  {:clauses
   '({:tag :init
      :values ({:tag :parallel-substitution
                 :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                 {:tag :assign :identifiers (:r) :values (13)})})})})

(def more-init-ir
  {:clauses
   '({:tag :init
      :values ({:tag :parallel-substitution
                 :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                 {:tag :assign :identifiers (:r) :values (13)}
                                 {:tag :assign :identifiers (:p) :values (3)})})})})

(def even-more-init-ir
  {:clauses
   '({:tag :init
      :values ({:tag :parallel-substitution
                 :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                 {:tag :assign :identifiers (:r) :values (13)}
                                 {:tag :assign :identifiers (:p) :values (3)}
                                 {:tag :assign :identifiers (:x) :values (15)}
                                 {:tag :assign :identifiers (:z) :values (:VAR)})})})})

(deftest init-utils-test
  (testing "Counting"
    (is (= 0 (util/count-inits no-init-ir)))
    (is (= 1 (util/count-inits single-init-ir)))
    (is (= 2 (util/count-inits multiple-init-ir)))
    (is (= 3 (util/count-inits more-init-ir)))
    (is (= 5 (util/count-inits even-more-init-ir))))
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

(def no-invar-u {:ir {:clauses '()} :a 3})
(def invar-u {:ir {:clauses '({:tag :invariants
                               :b 4})} :a 3})

(deftest invar-utils-test
  (testing "Getting"
    (is (nil? (util/get-invariant no-invar-u)))
    (is (seq (util/get-invariant invar-u))))
  (testing "Setting"
    (is (= invar-u (util/set-invariant no-invar-u {:tag :invariants
                                                   :b 4})))))
