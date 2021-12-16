(ns fset.predicates-test
  (:require
   [fset.backend :as b]
   [clojure.test :refer [deftest is]]
   [fset.test-machines :refer [train scheduler]]
   [fset.predicates :refer [unroll-predicate]]))


(def member-scheduler-expr {:tag :member :elem :PID1 :set :PID})

(b/setup-backend train)

(unroll-predicate member-scheduler-expr)


(def member-train-expr {:tag :member, :elem :R7, :set {:tag :difference :sets (list :resrt :frm)}})


(clojure.pprint/pprint (fset.simplify/simplify-all (unroll-predicate {:tag :member, :elem {:tag :fn-call, :f :fst, :args (list :R10)}, :set {:tag :difference, :sets (list :resbl :OCC)}})))
