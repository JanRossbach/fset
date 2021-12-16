(ns fset.config
  (:require [clojure.set :refer [difference]]
            [clojure.stacktrace :refer [print-throwable]]
            [clojure.pprint :refer [pprint]]))

;; Namespace to set global constants

(def logging true)

(def max-unroll-size 200)

(def unroll-constants false) ; Causes severe Performance loss, but can yield a simpler result in the end

(def deff-set-size 2)

(def train-vars #{:LBT :TRK :frm :OCC :resbl :resrt :rsrtbl})

(def train-vars-allowed #{:frm :resrt :resbl})

(def excluded-vars (difference train-vars train-vars-allowed))

(defn log [e]
  (if logging
    (do
      (pprint "Context: Assignment")
      (print-throwable e)
      (println)
      (println))
    nil))
