(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [fset.core :refer [transform]]))

(def empty-machine-str (slurp "resources/machines/test/b/empty.mch"))
(def empty-machine-lisb '(machine
                          (machine-variant)
                          (machine-header :Empty [])))

(def empty-cfg {:deferred-size 3 :max-transform-size 10})

(deftest empty-machine-test
  (testing "The empty machine should not be changed in any way"
      (is (= (transform empty-cfg empty-machine-lisb) empty-machine-str))))
