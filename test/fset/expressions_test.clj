(ns fset.expressions-test
  (:require
   [clojure.core.matrix :as m]
   [fset.backend :as b]
   [clojure.test :refer [deftest is]]
   [fset.test-machines :refer [train scheduler]]
   [fset.expressions :refer [unroll-expression bmmul]]))

(def image-expr {:tag :image, :rel {:tag :inverse, :rel :rtbl}, :set #{:R1}})
(def ran-expr  {:tag :difference, :sets (:resrt {:tag :ran, :rel :rsrtbl})})
(def fncall-expr {:tag :fn-call, :f :fst, :args '(:R10)})

(def image-expr-target {})
(def ran-expr-target {})
(def fncall-expr-target {})

(b/setup-backend scheduler)

(unroll-expression :PID)




(deftest train-relation-expressions-test
  (b/setup-backend train)
  (is (= image-expr-target (unroll-expression image-expr)))
  (is (= ran-expr-target (unroll-expression ran-expr)))
  (is (= fncall-expr-target (unroll-expression fncall-expr))))
