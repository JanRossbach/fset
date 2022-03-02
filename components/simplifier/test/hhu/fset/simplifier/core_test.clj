(ns hhu.fset.simplifier.core-test
  (:require [clojure.test :refer [deftest is are testing]]
            [lisb.translation.util :refer [ir->b b->ir]]
            [hhu.fset.simplifier.core :as core]))


(deftest simplify-formula-test
  (testing "Predicates"
    (are [x y] (= x (ir->b (core/simplify-all (b->ir (str "#PREDICATE" y)))))
      "TRUE=TRUE"
      "not(not(TRUE=TRUE))"

      "a=b"
      "a=b or TRUE=FALSE"

      "TRUE=TRUE"
      "a=b or TRUE=TRUE"

      "TRUE=FALSE"
      "a=b & TRUE=FALSE"

      "a=b"
      "a=b & TRUE=TRUE"

      "a=b"
      "TRUE=TRUE => a=b"

      "TRUE=TRUE"
      "TRUE=FALSE => a=b"

      "TRUE=TRUE"
      "a=b => TRUE=TRUE"

      "not(a=b)"
      "a=b => TRUE=FALSE"

      "not(a=b)"
      "a=b <=> TRUE=FALSE "

      "a=b"
      "a=b <=> TRUE=TRUE"))

  (testing "Expressions"
    (are [x y] (= x (ir->b (core/simplify-all (b->ir (str "#EXPRESSION" y)))))
      "TRUE"
      "bool(TRUE=TRUE)"

      "FALSE"
      "bool(TRUE=FALSE)"

      "A"
      "IF TRUE=TRUE THEN A ELSE B END"

      "B"
      "IF TRUE=FALSE THEN A ELSE B END")))
