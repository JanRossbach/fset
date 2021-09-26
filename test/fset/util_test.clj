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

(deftest get-machine-header-test
  (is (= (util/get-nodes-by-tag :machine-header empty-mch-ir)
         [{:tag :machine-header, :name :Empty, :parameters []}])))

(deftest update-machine-header-test
  (is (= (util/update-nodes-by-tag :machine-header #(assoc % :name :Full) empty-mch-ir)
         {:tag :machine, :variant {:tag :machine-variant}, :header
          {:tag :machine-header, :name :Full,
           :parameters []},
          :clauses nil})))

(deftest variables-test
  (testing "Adding and clearing variables from the IR"
    (is (= {:clauses
            '({:tag :variables
               :identifiers nil})}
           (util/clear-vars variables-ir)))
    (is (= {:clauses '({:tag :variables
                        :identifiers (:active :ready :waiting :hello :world)})}
           (util/add-vars variables-ir '(:hello :world)))))
  (testing "Adding vars to a machine without a vars clause will add a vars clause"
    (is (= {:clauses '({:tag :variables
                        :identifiers (:hello :world)})}
           (util/add-vars {:clauses '()} '(:hello :world))))))
