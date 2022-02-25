(ns hhu.fset.backend.core-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [hhu.fset.backend.core :as core]))

(def op
  {:tag :operations,
   :values
   '({:tag :op,
     :returns [],
     :name :Set,
     :args [:yy],
     :body
     {:tag :precondition,
      :pred {:tag :member, :elem :yy, :set :ID},
      :subs ({:tag :assignment, :id-vals (:xx :yy)})}})})
