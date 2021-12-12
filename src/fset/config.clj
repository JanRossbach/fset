(ns fset.config
  (:require [clojure.set :refer [difference]]))


;; Namespace to set global constants

(def max-unroll-size 200)

(def deff-set-size 2)

(def train-vars #{:LBT :TRK :frm :OCC :resbl :resrt :rsrtbl})

(def train-vars-allowed #{:frm :resrt})

(def excluded-vars (difference train-vars train-vars-allowed))
