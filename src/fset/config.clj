(ns fset.config
  (:require [clojure.set :refer [difference]]
            [clojure.stacktrace :refer [print-throwable]]
            [clojure.pprint :refer [pprint]]))

;; Namespace to set global constants

(def logging true)

(def max-unroll-size 200)

(def eval-constants true)

(def deff-set-size 2)

(def train-vars #{:LBT :TRK :frm :OCC :resbl :resrt :rsrtbl})

(def train-vars-allowed #{:frm :resrt :resbl :LBT :OCC})

(def excluded-vars (difference train-vars train-vars-allowed))


excluded-vars


(defn log [e context]
  (if logging
    (do
      (pprint "Context: ")
      (pprint context)
      (print-throwable e)
      (println)
      (println))
    nil))
