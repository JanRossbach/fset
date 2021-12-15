(ns fset.predicates-test
  (:require
   [fset.backend :as b]
   [clojure.test :refer [deftest is]]
   [fset.test-machines :refer [train scheduler]]
   [fset.predicates :refer [unroll-predicate]]))


(def member-scheduler-expr {:tag :member :elem :PID1 :set :PID})

(b/setup-backend train)

(unroll-predicate member-scheduler-expr)


(def member-train-expr {:tag :member, :elem :B, :set :OCC})

(unroll-predicate member-train-expr)
