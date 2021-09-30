(ns fset.extract-test
  (:require
   [fset.extract :as ex]
   [lisb.translation.util :refer [b->ir ir->b]]
   [clojure.test :refer [deftest is testing]]))

(def example-source-str "MACHINE fun_extract\nSETS\n  A = {VAR1, VAR2}\nVARIABLES\n  p\nINVARIANT\n  p : A --> 5..6\nINITIALISATION\n  p := {VAR1 |-> 5, VAR2 |-> 6}\nEND\n")
(def example-target-str "MACHINE fun_extract\nVARIABLES VAR1, VAR2\nINVARIANT VAR1 : 5..6 & VAR2 : 5..6\nINITIALISATION VAR1 := 5 || VAR2 := 6\nEND")

(deftest example-test
  (testing "The translated example is equal to the target string"
   (is (= example-target-str (ir->b (ex/extract-vars (b->ir example-source-str) '(:p)))))))
