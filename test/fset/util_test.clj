(ns fset.util-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.util :as util]))

(def variables-ir
  {:clauses
   '({:tag :variables
      :identifiers (:active :ready :waiting)})})

(deftest variables-test
  (testing "Adding variables to the IR"
    (is (= {:clauses '({:tag :variables
                        :identifiers (:active :ready :waiting :hello :world)})}
           (util/add-vars variables-ir '(:hello :world)))))
  (testing "Adding vars to a machine without a vars clause will add a vars clause"
    (is (= {:clauses '({:tag :variables
                        :identifiers (:hello :world)})}
           (util/add-vars {:clauses '()} '(:hello :world))))))

(def init-ir
  {:clauses
   '({:tag :init
      :substitution {:tag :parallel-substitution
                     :substitutions ({:tag :assign :identifiers (:q) :values (7)})}})})


(deftest init-test
  (testing "Getting"
    (is (= {:tag :init
            :substitution {:tag :parallel-substitution
                           :substitutions '({:tag :assign :identifiers (:q) :values (7)})}}
           (util/get-init init-ir)))
    (is (empty? (util/get-init variables-ir))))
  (testing "Adding"
    (is (= {:clauses
            '({:tag :init
               :substitution {:tag :parallel-substitution
                              :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                              {:tag :assign :identifiers (:p) :values (3)})}})}
           (util/add-inits init-ir [[:p 3]])))
    (is (= {:clauses
            '({:tag :init
               :substitution {:tag :parallel-substitution
                              :substitutions ({:tag :assign :identifiers (:q) :values (7)}
                                              {:tag :assign :identifiers (:p) :values (3)})}})}
           (util/add-inits {:clauses '({:tag :init
                                        :substitution {:tag :assign :identifiers (:q) :values (7)}})} [[:p 3]])))))
