(ns hhu.fset.backend.util
  (:require [clojure.core.match :refer [match]]))

(defn- id->str [id]
  (match id
         {:tag :maplet :left l :right r} (str (id->str l) (id->str r))
         (k :guard keyword?) (name k)))

(defn create-boolname [& ids]
  (keyword (apply str (map id->str ids))))
