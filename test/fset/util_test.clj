(ns fset.util-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.util :as util]))

(def empty-mch-ir
  {:tag :machine
   :variant {:tag :machine-variant}
   :header {:tag :machine-header, :name :Empty, :parameters []}, :clauses nil})

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
