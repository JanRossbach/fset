(ns fset.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [lisb.translation.util :refer [lisb->ir]]
   [fset.core :refer [transform]]))

(def empty-machine-str (slurp "resources/machines/test/b/empty.mch"))
(def empty-machine-lisb '(machine
                          (machine-variant)
                          (machine-header :Empty [])))

(def empty-machine-ir (lisb->ir empty-machine-lisb))

(clojure.pprint/pprint empty-machine-ir)


(deftest empty-machine-test
  (testing "The empty machine should not be changed in any way"
      (is (= (transform empty-machine-lisb) empty-machine-str))))
