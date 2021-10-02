(ns fset.util-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.util :as util]))

(def variables-ir
  {:clauses
   '({:tag :variables
      :identifiers (:active :ready :waiting)})})

(deftest variable-utils-test
  (testing "Adding variables to the IR"
    (is (= {:clauses '({:tag :variables
                        :identifiers (:active :ready :waiting :hello :world)})}
           (util/add-vars variables-ir '(:hello :world)))))
  (testing "Adding vars to a machine without a vars clause will add a vars clause"
    (is (= {:clauses '({:tag :variables
                        :identifiers (:hello :world)})}
           (util/add-vars {:clauses '()} '(:hello :world))))))


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
    (is (empty? (util/get-init variables-ir))))
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
    (is (= single-init-ir (util/rm-inits-by-id more-init-ir :p :r)))))
