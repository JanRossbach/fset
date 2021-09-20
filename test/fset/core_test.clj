(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [li]
   [fset.core :refer [transform]]))

(def empty-machine-str (slurp "resources/machines/test/b/empty.mch"))
(def empty-machine-lisb '(machine
                          (machine-variant)
                          (machine-header :Empty [])))

(deftest empty-machine-test
  (testing "The empty machine should not be changed in any way"
      (is (= (transform empty-machine-lisb) empty-machine-str))))
